namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestMethodTableProjection =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private stateWithLogger (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private state () : IlMachineState =
        // Factory intentionally undisposed: state.Logger outlives this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()

        stateWithLogger loggerFactory

    let private topLevelType (``namespace`` : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef ``namespace`` name with
        | None -> failwith $"%s{``namespace``}.%s{name} not found in corelib"
        | Some typeInfo -> typeInfo

    let private openGenericProjectionAssembly : DumpedAssembly =
        let source =
            """
namespace PawPrint.MethodTableProjection;

public struct PlainValue
{
    public int Number;
}

public class BaseWithObject
{
    public object Ref;
}

public class OpenWithPlainValue<T>
{
    public PlainValue Value;
}

public class OpenWithGenericField<T>
{
    public T Value;
}

public class OpenDerivedFromBase<T> : BaseWithObject
{
    public int Number;
}

public struct OpenStruct<T>
{
    public int Number;
}

public interface IOpenInterface<T>
{
    void DoSomething(T value);
}
"""

        let bytes =
            Roslyn.compileAssembly
                "PawPrint.MethodTableProjection"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        use stream = new MemoryStream (bytes)
        let _, loggerFactory = LoggerFactory.makeTest ()

        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory (Some "PawPrint.MethodTableProjection.dll") stream

    let private openGenericProjectionType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        match openGenericProjectionAssembly.TryGetTopLevelTypeDef "PawPrint.MethodTableProjection" name with
        | None -> failwith $"PawPrint.MethodTableProjection.%s{name} not found in projection test assembly"
        | Some typeInfo -> typeInfo

    let private methodTableField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTable" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTable not found in corelib"
        | Some methodTable ->
            methodTable.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTable::{name} not found")

    let private methodTableAuxiliaryDataField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "MethodTableAuxiliaryData" with
        | None -> failwith "System.Runtime.CompilerServices.MethodTableAuxiliaryData not found in corelib"
        | Some methodTableAuxiliaryData ->
            methodTableAuxiliaryData.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"MethodTableAuxiliaryData::{name} not found")

    let private rawArrayDataField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RawArrayData" with
        | None -> failwith "System.Runtime.CompilerServices.RawArrayData not found in corelib"
        | Some rawArrayData ->
            rawArrayData.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"RawArrayData::{name} not found")

    let private rawDataField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        match corelib.TryGetTopLevelTypeDef "System.Runtime.CompilerServices" "RawData" with
        | None -> failwith "System.Runtime.CompilerServices.RawData not found in corelib"
        | Some rawData ->
            rawData.Fields
            |> List.tryFind (fun field -> field.Name = name)
            |> Option.defaultWith (fun () -> failwith $"RawData::{name} not found")

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private concreteTypeFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteType<ConcreteTypeHandle> =
        handleFor ti
        |> fun handle -> AllConcreteTypes.lookup handle concreteTypes
        |> Option.defaultWith (fun () -> failwith $"Could not find concrete type for %O{ti}")

    let private intPtrValueField () : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        bct.IntPtr.Fields
        |> List.filter (fun field -> field.Name = "_value" && not field.IsStatic)
        |> List.exactlyOne

    let private intPtrValueFieldId () : FieldId =
        let intPtrHandle = handleFor bct.IntPtr
        let valueField = intPtrValueField ()

        FieldId.metadata intPtrHandle valueField.Handle valueField.Name

    let private int32StaticField (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        bct.Int32.Fields
        |> List.tryFind (fun field -> field.Name = name && field.IsStatic)
        |> Option.defaultWith (fun () -> failwith $"System.Int32::{name} static field not found")

    let private allocateIntArray (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        IlMachineState.allocateArray intArrayHandle (fun () -> CliType.Numeric (CliNumericType.Int32 0)) length state

    let private allocateInt64Array (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let int64ArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int64)

        IlMachineState.allocateArray
            int64ArrayHandle
            (fun () -> CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
            length
            state

    let private allocateBoxedIntPtr (bits : int64) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let intPtrHandle = handleFor bct.IntPtr
        let valueField = intPtrValueField ()

        let valueType =
            [
                {
                    Id = intPtrValueFieldId ()
                    Name = valueField.Name
                    Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits))
                    Offset = valueField.Offset
                    Type = intPtrHandle
                    MarshallingDescriptor = None
                }
            ]
            |> CliValueType.OfFields bct state.ConcreteTypes intPtrHandle Layout.Default CharSet.Ansi

        IlMachineState.allocateManagedObject intPtrHandle valueType state

    let private allocateReferenceObject (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let objectHandle = handleFor bct.Object

        let objectValue =
            CliValueType.OfFields bct state.ConcreteTypes objectHandle Layout.Default CharSet.Ansi []

        IlMachineState.allocateManagedObject objectHandle objectValue state

    // Deliberately synthetic value types: these tests pin storage-shape guards for object and
    // runtime-pointer payloads, not metadata identity or real corelib layout.
    let private objectReferenceValueType (state : IlMachineState) : CliValueType * IlMachineState =
        let declared = handleFor bct.TypedReference
        let objectHandle = handleFor bct.Object
        let objectAddr, state = allocateReferenceObject state

        let valueType =
            [
                {
                    Id = FieldId.named "Obj"
                    Name = "Obj"
                    Contents = CliType.ObjectRef (Some objectAddr)
                    Offset = Some 0
                    Type = objectHandle
                    MarshallingDescriptor = None
                }
            ]
            |> CliValueType.OfFields
                bct
                state.ConcreteTypes
                declared
                (Layout.Custom (size = 8, packingSize = 0))
                CharSet.Ansi

        valueType, state

    let private runtimePointerValueType (state : IlMachineState) : CliValueType =
        let declared = handleFor bct.TypedReference
        let intPtrHandle = handleFor bct.IntPtr
        let intHandle = handleFor bct.Int32

        [
            {
                Id = FieldId.named "Ptr"
                Name = "Ptr"
                Contents =
                    CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed intHandle))
                Offset = Some 0
                Type = intPtrHandle
                MarshallingDescriptor = None
            }
        ]
        |> CliValueType.OfFields
            bct
            state.ConcreteTypes
            declared
            (Layout.Custom (size = 8, packingSize = 0))
            CharSet.Ansi

    let private allocateObjectReferenceValue (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let valueType, state = objectReferenceValueType state

        IlMachineState.allocateManagedObject valueType.Declared valueType state

    let private allocateRuntimePointerValue (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let valueType = runtimePointerValueType state

        IlMachineState.allocateManagedObject valueType.Declared valueType state

    /// Allocate a reference-type heap object whose storage carries reference fields. Real CLR
    /// classes (e.g. EventSource's `OverrideEventProvider`) have this shape: declared identity
    /// is a reference type, but instance storage holds object-typed fields. The companion stored
    /// address is what `Ref` initially points at, so tests can assert byte-view reads recover
    /// the original reference rather than zero-padding it.
    let private allocateReferenceObjectWithRefField
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedHeapAddress * IlMachineState
        =
        let storedAddr, state = allocateReferenceObject state
        let objectHandle = handleFor bct.Object

        let fields =
            [
                {
                    Id = FieldId.named "Ref"
                    Name = "Ref"
                    Contents = CliType.ObjectRef (Some storedAddr)
                    Offset = Some 0
                    Type = objectHandle
                    MarshallingDescriptor = None
                }
            ]
            |> CliValueType.OfFields
                bct
                state.ConcreteTypes
                objectHandle
                (Layout.Custom (size = 8, packingSize = 0))
                CharSet.Ansi

        let containerAddr, state =
            IlMachineState.allocateManagedObject objectHandle fields state

        storedAddr, containerAddr, state

    let private allocateSingleValueTypeArray
        (valueType : CliValueType)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let arrayType = ConcreteTypeHandle.OneDimArrayZero valueType.Declared

        IlMachineState.allocateArray arrayType (fun () -> CliType.ValueType valueType) 1 state

    let private projectRawDataDataPointer (addr : ManagedHeapAddress) (state : IlMachineState) : ManagedPointerSource =
        RuntimeFieldProjection.tryProjectFieldAddress bct (rawDataField "Data") addr state
        |> Option.defaultWith (fun () -> failwith "Expected RawData::Data to project")

    let private boxedPayloadBytes (addr : ManagedHeapAddress) (state : IlMachineState) : byte[] =
        ManagedHeap.get addr state.ManagedHeap |> _.Contents |> CliValueType.ToBytes

    let private boxedPayloadValueType (addr : ManagedHeapAddress) (state : IlMachineState) : CliValueType =
        ManagedHeap.get addr state.ManagedHeap |> _.Contents

    let private arrayElementValueType
        (addr : ManagedHeapAddress)
        (index : int)
        (state : IlMachineState)
        : CliValueType
        =
        match state.ManagedHeap.Arrays.[addr].Elements.[index] with
        | CliType.ValueType vt -> vt
        | other -> failwith $"Expected array element %d{index} at %O{addr} to be a value type, got %O{other}"

    let private assertReadWriteByteViewRejected
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (expectedFragments : string list)
        : unit
        =
        let readEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.UInt8 0uy))
                |> ignore
            )

        for fragment in expectedFragments do
            readEx.Message |> shouldContainText fragment

        let writeEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    ptr
                    (CliType.Numeric (CliNumericType.UInt8 0xAAuy))
                |> ignore
            )

        for fragment in expectedFragments do
            writeEx.Message |> shouldContainText fragment

    type private RawDataWriteCase =
        {
            Initial : int64
            Offset : int
            Payload : uint16
        }

    type private ByteIdenticalUInt16WriteCase =
        {
            Initial : int64
            Offset : int
        }

    type private ByteIdenticalFieldWriteCase =
        {
            Initial : int64
            Offset : int
        }

    type private SignedZeroWriteCase =
        {
            InitialNegative : bool
            WrittenNegative : bool
        }

    [<RequireQualifiedAccess>]
    type private TaggedNativeIntDestination =
        | StackMemory
        | NativeIntArrayElement
        | IntPtrField

    [<RequireQualifiedAccess>]
    type private TaggedInt64Destination =
        | StackMemory
        | Int64ArrayElement

    let private rawDataPropertyConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 200

    let private genRawDataWriteCase : Gen<RawDataWriteCase> =
        gen {
            let! initial = ArbMap.defaults |> ArbMap.generate<int64>
            let! offset = Gen.choose (0, 6)
            let! payload = ArbMap.defaults |> ArbMap.generate<uint16>

            return
                {
                    Initial = initial
                    Offset = offset
                    Payload = payload
                }
        }

    let private genByteIdenticalUInt16WriteCase : Gen<ByteIdenticalUInt16WriteCase> =
        gen {
            let! initial = ArbMap.defaults |> ArbMap.generate<int64>
            let! offset = Gen.choose (0, 6)

            return
                {
                    Initial = initial
                    Offset = offset
                }
        }

    let private genByteIdenticalFieldWriteCase : Gen<ByteIdenticalFieldWriteCase> =
        gen {
            let! initial = ArbMap.defaults |> ArbMap.generate<int64>
            let! offset = Gen.choose (0, 7)

            return
                {
                    Initial = initial
                    Offset = offset
                }
        }

    let private genSignedZeroWriteCase : Gen<SignedZeroWriteCase> =
        gen {
            let! initialNegative = ArbMap.defaults |> ArbMap.generate<bool>
            let! writtenNegative = ArbMap.defaults |> ArbMap.generate<bool>

            return
                {
                    InitialNegative = initialNegative
                    WrittenNegative = writtenNegative
                }
        }

    let private signedZero (negative : bool) : float = if negative then -0.0 else 0.0

    let private reinterpretWriteAssembly : DumpedAssembly =
        let source =
            """
namespace PawPrint.ReinterpretWrite;

public struct Int32Wrapper
{
    public int Value;
}

public struct FourBytes
{
    public byte B0;
    public byte B1;
    public byte B2;
    public byte B3;
}

public struct ByteWrapper
{
    public byte Value;
}

public struct FourByteWrappers
{
    public ByteWrapper B0;
    public ByteWrapper B1;
    public ByteWrapper B2;
    public ByteWrapper B3;
}

public unsafe struct PointerWrapper
{
    public void* Ptr;
}
"""

        let bytes =
            Roslyn.compileAssembly
                "PawPrint.ReinterpretWrite"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        use stream = new MemoryStream (bytes)
        let _, loggerFactory = LoggerFactory.makeTest ()

        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory (Some "PawPrint.ReinterpretWrite.dll") stream

    type private ReinterpretWriteTypes =
        {
            State : IlMachineState
            Int32WrapperHandle : ConcreteTypeHandle
            Int32WrapperValueField : FieldId
            FourBytesConcrete : ConcreteType<ConcreteTypeHandle>
            FourBytesFields : FieldId[]
            ByteWrapperConcrete : ConcreteType<ConcreteTypeHandle>
            ByteWrapperValueField : FieldId
            FourByteWrappersConcrete : ConcreteType<ConcreteTypeHandle>
            FourByteWrapperFields : FieldId[]
            PointerWrapperConcrete : ConcreteType<ConcreteTypeHandle>
            PointerWrapperPtrField : FieldId
        }

    type private ReinterpretByteWriteCase =
        {
            Initial : int32
            Payloads : byte[]
        }

    let private reinterpretWritePropertyConfig : Config =
        Config.QuickThrowOnFailure.WithMaxTest 200

    let private genReinterpretByteWriteCase : Gen<ReinterpretByteWriteCase> =
        gen {
            let! initial = ArbMap.defaults |> ArbMap.generate<int32>
            let! payload0 = ArbMap.defaults |> ArbMap.generate<byte>
            let! payload1 = ArbMap.defaults |> ArbMap.generate<byte>
            let! payload2 = ArbMap.defaults |> ArbMap.generate<byte>
            let! payload3 = ArbMap.defaults |> ArbMap.generate<byte>

            return
                {
                    Initial = initial
                    Payloads = [| payload0 ; payload1 ; payload2 ; payload3 |]
                }
        }

    let private reinterpretWriteType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        reinterpretWriteAssembly.TryGetTopLevelTypeDef "PawPrint.ReinterpretWrite" name
        |> Option.defaultWith (fun () -> failwith $"PawPrint.ReinterpretWrite.%s{name} not found")

    let private concretizeReinterpretWriteType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle * ConcreteType<ConcreteTypeHandle>
        =
        let state, handle =
            typeInfo
            |> DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                state
                reinterpretWriteAssembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let concrete =
            AllConcreteTypes.lookup handle state.ConcreteTypes
            |> Option.defaultWith (fun () -> failwith $"Missing concrete type for %O{typeInfo}")

        state, handle, concrete

    let private instanceField
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (name : string)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        typeInfo.Fields
        |> List.tryFind (fun field -> field.Name = name && not field.IsStatic)
        |> Option.defaultWith (fun () -> failwith $"%s{typeInfo.Name}::%s{name} not found")

    let private fieldId
        (declaringType : ConcreteTypeHandle)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        : FieldId
        =
        FieldId.metadata declaringType field.Handle field.Name

    let private reinterpretWriteTypes
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        : ReinterpretWriteTypes
        =
        let state =
            stateWithLogger loggerFactory
            |> fun state -> state.WithLoadedAssembly reinterpretWriteAssembly.Name reinterpretWriteAssembly

        let int32Wrapper = reinterpretWriteType "Int32Wrapper"
        let fourBytes = reinterpretWriteType "FourBytes"
        let byteWrapper = reinterpretWriteType "ByteWrapper"
        let fourByteWrappers = reinterpretWriteType "FourByteWrappers"
        let pointerWrapper = reinterpretWriteType "PointerWrapper"

        let state, int32WrapperHandle, _int32WrapperConcrete =
            concretizeReinterpretWriteType loggerFactory state int32Wrapper

        let state, fourBytesHandle, fourBytesConcrete =
            concretizeReinterpretWriteType loggerFactory state fourBytes

        let state, byteWrapperHandle, byteWrapperConcrete =
            concretizeReinterpretWriteType loggerFactory state byteWrapper

        let state, fourByteWrappersHandle, fourByteWrappersConcrete =
            concretizeReinterpretWriteType loggerFactory state fourByteWrappers

        let state, pointerWrapperHandle, pointerWrapperConcrete =
            concretizeReinterpretWriteType loggerFactory state pointerWrapper

        {
            State = state
            Int32WrapperHandle = int32WrapperHandle
            Int32WrapperValueField = instanceField int32Wrapper "Value" |> fieldId int32WrapperHandle
            FourBytesConcrete = fourBytesConcrete
            FourBytesFields =
                [| "B0" ; "B1" ; "B2" ; "B3" |]
                |> Array.map (fun name -> instanceField fourBytes name |> fieldId fourBytesHandle)
            ByteWrapperConcrete = byteWrapperConcrete
            ByteWrapperValueField = instanceField byteWrapper "Value" |> fieldId byteWrapperHandle
            FourByteWrappersConcrete = fourByteWrappersConcrete
            FourByteWrapperFields =
                [| "B0" ; "B1" ; "B2" ; "B3" |]
                |> Array.map (fun name -> instanceField fourByteWrappers name |> fieldId fourByteWrappersHandle)
            PointerWrapperConcrete = pointerWrapperConcrete
            PointerWrapperPtrField = instanceField pointerWrapper "Ptr" |> fieldId pointerWrapperHandle
        }

    let private allocateInt32Wrapper
        (types : ReinterpretWriteTypes)
        (initial : int32)
        : ManagedHeapAddress * IlMachineState
        =
        let zero, state =
            IlMachineState.cliTypeZeroOfHandle types.State bct types.Int32WrapperHandle

        let contents =
            match zero with
            | CliType.ValueType vt ->
                CliValueType.WithFieldSetById
                    types.Int32WrapperValueField
                    (CliType.Numeric (CliNumericType.Int32 initial))
                    vt
            | other -> failwith $"Int32Wrapper zero was not a value type: %O{other}"

        IlMachineState.allocateManagedObject types.Int32WrapperHandle contents state

    let private wrapperValue
        (types : ReinterpretWriteTypes)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : int32
        =
        match
            ManagedHeap.get addr state.ManagedHeap
            |> _.Contents
            |> CliValueType.DereferenceFieldById types.Int32WrapperValueField
        with
        | CliType.Numeric (CliNumericType.Int32 value) -> value
        | other -> failwith $"Int32Wrapper::Value was not Int32: %O{other}"

    let private writeFourBytesField
        (types : ReinterpretWriteTypes)
        (fieldIndex : int)
        (replacement : byte)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState
        =
        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [
                    ByrefProjection.ReinterpretAs types.FourBytesConcrete
                    ByrefProjection.Field types.FourBytesFields.[fieldIndex]
                ]
            )

        IlMachineState.writeManagedByrefWithBase bct state ptr (CliType.Numeric (CliNumericType.UInt8 replacement))

    let private hasComponentSizeFlag : int32 = int32 0x80000000u
    let private containsGcPointersFlag : int32 = 0x01000000
    let private categoryMask : int32 = 0x000F0000
    let private categoryInterface : int32 = 0x000C0000
    let private categoryArray : int32 = 0x00080000
    let private categoryValueType : int32 = 0x00040000
    let private componentSizeMask : int32 = 0x0000FFFF
    let private genericsMask : int32 = 0x00000030
    let private genericsTypicalInst : int32 = 0x00000030
    let private containsGenericVariablesFlag : int32 = 0x20000000

    let private projectFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (fieldName : string)
        (target : ConcreteTypeHandle)
        : CliType * IlMachineState
        =
        match MethodTableProjection.tryProjectField loggerFactory bct (methodTableField fieldName) target state with
        | None -> failwith $"Expected MethodTable::{fieldName} to project"
        | Some result -> result

    let private projectWithState (fieldName : string) (target : ConcreteTypeHandle) : CliType * IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        projectFromState loggerFactory (stateWithLogger loggerFactory) fieldName target

    let private project (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        // Current cases use already-concretized corelib shapes; non-primitive value-type elements should assert state too.
        projectWithState fieldName target |> fst

    let private projectFlags (target : ConcreteTypeHandle) : int32 =
        match project "Flags" target with
        | CliType.Numeric (CliNumericType.Int32 flags) -> flags
        | other -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

    let private projectFlagsFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (target : ConcreteTypeHandle)
        : int32
        =
        match projectFromState loggerFactory state "Flags" target with
        | CliType.Numeric (CliNumericType.Int32 flags), _ -> flags
        | other, _ -> failwith $"Expected MethodTable::Flags as Int32, got %O{other}"

    let private concretizeCorelibType
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        typeInfo
        |> DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies
        |> IlMachineState.concretizeType loggerFactory bct state corelib.Name ImmutableArray.Empty ImmutableArray.Empty

    let private projectAuxiliaryData (fieldName : string) (target : ConcreteTypeHandle) : CliType =
        match
            MethodTableProjection.tryProjectAuxiliaryDataField
                bct
                (methodTableAuxiliaryDataField fieldName)
                (RuntimeTypeHandleTarget.Closed target)
                (state ())
        with
        | None -> failwith $"Expected MethodTableAuxiliaryData::{fieldName} to project"
        | Some (result, _) -> result

    let private methodWithSingleInstructionAndLocals
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (localVars : ImmutableArray<ConcreteTypeHandle>)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let objectToString =
            bct.Object.Methods
            |> List.find (fun method -> method.Name = "ToString" && method.Parameters.IsEmpty)

        let state, signature =
            TypeMethodSignature.map
                state
                (fun state ty ->
                    IlMachineState.concretizeType
                        loggerFactory
                        bct
                        state
                        corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        ty
                )
                objectToString.Signature

        let instructions : MethodInstructions<ConcreteTypeHandle> =
            { MethodInstructions.onlyRet () with
                Instructions = [ op, 0 ]
                Locations = Map.empty |> Map.add 0 op
                LocalVars = if localVars.IsEmpty then None else Some localVars
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (MethodBody.Il instructions) signature

        state, method

    let private methodWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (state : IlMachineState)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        methodWithSingleInstructionAndLocals loggerFactory op ImmutableArray.Empty state

    let private stateWithSingleInstructionAndLocalsFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (localVars : ImmutableArray<ConcreteTypeHandle>)
        (initialState : IlMachineState)
        : IlMachineState * ThreadId
        =
        let state, method =
            initialState |> methodWithSingleInstructionAndLocals loggerFactory op localVars

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    bct
                    state._LoadedAssemblies
                    corelib
                    method
                    ImmutableArray.Empty
                    (ImmutableArray.Create (CliType.ObjectRef None))
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith $"Unexpected missing assembly references creating MethodTableProjection frame: %O{missing}"

        let thread = ThreadId.ThreadId 0

        { state with
            ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
        },
        thread

    let private stateWithSingleInstructionAndLocals
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (localVars : ImmutableArray<ConcreteTypeHandle>)
        : IlMachineState * ThreadId
        =
        state ()
        |> stateWithSingleInstructionAndLocalsFromState loggerFactory op localVars

    let private stateWithSingleInstructionFromState
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        (initialState : IlMachineState)
        : IlMachineState * ThreadId
        =
        initialState
        |> stateWithSingleInstructionAndLocalsFromState loggerFactory op ImmutableArray.Empty

    let private stateWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
        : IlMachineState * ThreadId
        =
        stateWithSingleInstructionAndLocals loggerFactory op ImmutableArray.Empty

    let private ldfldMethodTableFlagsFromRuntimeTypeHandle
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (initialState : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : int32
        =
        let field = methodTableField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)

        let state, thread =
            initialState |> stateWithSingleInstructionFromState loggerFactory op

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr target)) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.Int32 flags) -> flags
        | other -> failwith $"Expected MethodTable::Flags on stack, got %O{other}"

    let private syntheticCrossStorageNativeIntSource () : NativeIntSource =
        NativeIntSource.syntheticCrossStorageByteOffset
            (ByteStorageIdentity.StackMemory (ThreadId 0, FrameId 0, StackMemoryBlockId 0))
            0L
            (ByteStorageIdentity.StackLocal (ThreadId 0, FrameId 0, 0us))
            8L

    let private functionPointerSource () : NativeIntSource =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let _, method =
            state ()
            |> methodWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Ret)

        NativeIntSource.FunctionPointer method

    let private taggedNativeIntSources () : NativeIntSource list =
        [
            NativeIntSource.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.StackMemoryByte (ThreadId 0, FrameId 0, StackMemoryBlockId 0, 0),
                    []
                )
            )
            functionPointerSource ()
            NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (handleFor bct.Int32))
            NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (handleFor bct.Int32))
            NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed (handleFor bct.Int32))
            NativeIntSource.MethodHandlePtr 1234L
            NativeIntSource.FieldHandlePtr 5678L
            NativeIntSource.AssemblyHandle "test-assembly"
            NativeIntSource.ModuleHandle "test-module"
            NativeIntSource.MetadataImportHandle "test-metadata-import"
            NativeIntSource.GcHandlePtr (GcHandleAddress 42)
            syntheticCrossStorageNativeIntSource ()
        ]

    let private taggedInt64Sources () : Int64Source list =
        taggedNativeIntSources ()
        |> List.collect (fun source ->
            [
                Int64Source.widenedNativeInt source true
                Int64Source.widenedNativeInt source false
            ]
        )
        |> List.distinct

    let private genTaggedNativeIntStindCase : Gen<NativeIntSource * TaggedNativeIntDestination> =
        gen {
            let! source = Gen.elements (taggedNativeIntSources ())

            let! destination =
                Gen.elements
                    [
                        TaggedNativeIntDestination.StackMemory
                        TaggedNativeIntDestination.NativeIntArrayElement
                        TaggedNativeIntDestination.IntPtrField
                    ]

            return source, destination
        }

    let private genTaggedInt64StindCase : Gen<Int64Source * TaggedInt64Destination> =
        gen {
            let! source = Gen.elements (taggedInt64Sources ())

            let! destination =
                Gen.elements
                    [
                        TaggedInt64Destination.StackMemory
                        TaggedInt64Destination.Int64ArrayElement
                    ]

            return source, destination
        }

    [<Test>]
    let ``BaseSize distinguishes szarrays from multidimensional arrays`` () : unit =
        let intHandle = handleFor bct.Int32

        project "BaseSize" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (3 * NATIVE_INT_SIZE)))

        project "BaseSize" (ConcreteTypeHandle.Array (intHandle, 2))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (5 * NATIVE_INT_SIZE)))

    [<Test>]
    let ``ComponentSize is computed from the structured element type`` () : unit =
        project "ComponentSize" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 4us))

        project "ComponentSize" (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 (uint16 NATIVE_INT_SIZE)))

        project "ComponentSize" (handleFor bct.String)
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 2us))

    [<Test>]
    let ``Flags identify array component size and GC pointer containment`` () : unit =
        let intArrayFlags =
            projectFlags (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32))

        let objectArrayFlags =
            projectFlags (ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object))

        let stringFlags = projectFlags (handleFor bct.String)

        intArrayFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag
        intArrayFlags &&& containsGcPointersFlag |> shouldEqual 0
        intArrayFlags &&& categoryMask |> shouldEqual categoryArray
        intArrayFlags &&& componentSizeMask |> shouldEqual 4

        objectArrayFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag

        objectArrayFlags &&& containsGcPointersFlag
        |> shouldEqual containsGcPointersFlag

        objectArrayFlags &&& categoryMask |> shouldEqual categoryArray
        objectArrayFlags &&& componentSizeMask |> shouldEqual NATIVE_INT_SIZE

        stringFlags &&& hasComponentSizeFlag |> shouldEqual hasComponentSizeFlag
        stringFlags &&& containsGcPointersFlag |> shouldEqual 0
        stringFlags &&& componentSizeMask |> shouldEqual 2

    [<Test>]
    let ``Flags compute non-array reference type GC pointer containment from instance fields`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = stateWithLogger loggerFactory

        let state, exceptionHandle = concretizeCorelibType loggerFactory state bct.Exception

        let state, disposableHandle =
            concretizeCorelibType loggerFactory state (topLevelType "System" "IDisposable")

        let objectFlags = projectFlags (handleFor bct.Object)
        let exceptionFlags = projectFlagsFromState loggerFactory state exceptionHandle
        let disposableFlags = projectFlagsFromState loggerFactory state disposableHandle

        objectFlags &&& hasComponentSizeFlag |> shouldEqual 0
        objectFlags &&& containsGcPointersFlag |> shouldEqual 0
        objectFlags &&& categoryMask |> shouldEqual 0

        exceptionFlags &&& hasComponentSizeFlag |> shouldEqual 0

        exceptionFlags &&& containsGcPointersFlag |> shouldEqual containsGcPointersFlag

        exceptionFlags &&& categoryMask |> shouldEqual 0

        disposableFlags &&& hasComponentSizeFlag |> shouldEqual 0
        disposableFlags &&& containsGcPointersFlag |> shouldEqual 0
        disposableFlags &&& categoryMask |> shouldEqual categoryInterface

    [<Test>]
    let ``Ldfld projects MethodTable flags from MethodTable pointer provenance`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let field = methodTableField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op

        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed intArrayHandle)
                ))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 (hasComponentSizeFlag ||| categoryArray ||| 4)))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldfld projects MethodTable flags from open generic type handles`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let target =
            topLevelType "System.Collections.Generic" "List`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags =
            ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory (stateWithLogger loggerFactory) target

        flags &&& hasComponentSizeFlag |> shouldEqual 0
        flags &&& genericsMask |> shouldEqual genericsTypicalInst
        flags &&& containsGcPointersFlag |> shouldEqual containsGcPointersFlag
        flags &&& categoryMask |> shouldEqual 0

        flags &&& containsGenericVariablesFlag
        |> shouldEqual containsGenericVariablesFlag

    [<Test>]
    let ``Open generic MethodTable flags inspect value-type fields precisely`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state =
            (stateWithLogger loggerFactory).WithLoadedAssembly
                openGenericProjectionAssembly.Name
                openGenericProjectionAssembly

        let target =
            openGenericProjectionType "OpenWithPlainValue`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags = ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory state target

        flags &&& hasComponentSizeFlag |> shouldEqual 0
        flags &&& containsGcPointersFlag |> shouldEqual 0
        flags &&& categoryMask |> shouldEqual 0
        flags &&& genericsMask |> shouldEqual genericsTypicalInst

        flags &&& containsGenericVariablesFlag
        |> shouldEqual containsGenericVariablesFlag

    [<Test>]
    let ``Open generic MethodTable flags treat unbound generic fields as maybe GC`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state =
            (stateWithLogger loggerFactory).WithLoadedAssembly
                openGenericProjectionAssembly.Name
                openGenericProjectionAssembly

        let target =
            openGenericProjectionType "OpenWithGenericField`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags = ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory state target

        flags &&& containsGcPointersFlag |> shouldEqual containsGcPointersFlag
        flags &&& genericsMask |> shouldEqual genericsTypicalInst

    [<Test>]
    let ``Open generic MethodTable flags include inherited instance fields`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state =
            (stateWithLogger loggerFactory).WithLoadedAssembly
                openGenericProjectionAssembly.Name
                openGenericProjectionAssembly

        let target =
            openGenericProjectionType "OpenDerivedFromBase`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags = ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory state target

        flags &&& containsGcPointersFlag |> shouldEqual containsGcPointersFlag
        flags &&& genericsMask |> shouldEqual genericsTypicalInst

    [<Test>]
    let ``Open generic struct MethodTable flags carry value-type category`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state =
            (stateWithLogger loggerFactory).WithLoadedAssembly
                openGenericProjectionAssembly.Name
                openGenericProjectionAssembly

        let target =
            openGenericProjectionType "OpenStruct`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags = ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory state target

        flags &&& hasComponentSizeFlag |> shouldEqual 0
        flags &&& containsGcPointersFlag |> shouldEqual 0
        flags &&& categoryMask |> shouldEqual categoryValueType
        flags &&& genericsMask |> shouldEqual genericsTypicalInst

        flags &&& containsGenericVariablesFlag
        |> shouldEqual containsGenericVariablesFlag

    [<Test>]
    let ``Open generic interface MethodTable flags carry interface category`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let state =
            (stateWithLogger loggerFactory).WithLoadedAssembly
                openGenericProjectionAssembly.Name
                openGenericProjectionAssembly

        let target =
            openGenericProjectionType "IOpenInterface`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let flags = ldfldMethodTableFlagsFromRuntimeTypeHandle loggerFactory state target

        flags &&& hasComponentSizeFlag |> shouldEqual 0
        flags &&& containsGcPointersFlag |> shouldEqual 0
        flags &&& categoryMask |> shouldEqual categoryInterface
        flags &&& genericsMask |> shouldEqual genericsTypicalInst

        flags &&& containsGenericVariablesFlag
        |> shouldEqual containsGenericVariablesFlag

    [<Test>]
    let ``ElementType preserves MethodTable pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "ElementType" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (
            CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed intHandle))
        )

    [<Test>]
    let ``AuxiliaryData preserves MethodTable auxiliary-data pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "AuxiliaryData" intHandle
        |> shouldEqual (
            CliType.RuntimePointer (
                CliRuntimePointer.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed intHandle)
            )
        )

    [<Test>]
    let ``AuxiliaryData preserves MethodTable auxiliary-data pointer provenance for open generic`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let target =
            topLevelType "System.Collections.Generic" "List`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let projected =
            match
                MethodTableProjection.tryProjectFieldForRuntimeTypeHandleTarget
                    loggerFactory
                    bct
                    (methodTableField "AuxiliaryData")
                    target
                    (stateWithLogger loggerFactory)
            with
            | None -> failwith "Expected MethodTable::AuxiliaryData to project"
            | Some (result, _) -> result

        projected
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTableAuxiliaryDataPtr target))

    [<Test>]
    let ``AuxiliaryData flags start with fast-compare cache bits unset`` () : unit =
        projectAuxiliaryData "Flags" (handleFor bct.Int32)
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

    let private concretizeNullableOf
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (argHandle : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        topLevelType "System" "Nullable`1"
        |> DumpedAssembly.typeInfoToTypeDefn' bct state._LoadedAssemblies
        |> IlMachineState.concretizeType
            loggerFactory
            bct
            state
            corelib.Name
            (ImmutableArray.Create argHandle)
            ImmutableArray.Empty

    [<Test>]
    let ``PerInstInfo projection succeeds for System.Nullable`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let intHandle = handleFor bct.Int32

        let state, nullableIntHandle =
            concretizeNullableOf loggerFactory intHandle (stateWithLogger loggerFactory)

        let projected, _ =
            projectFromState loggerFactory state "PerInstInfo" nullableIntHandle

        projected
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.PerInstInfoPtr nullableIntHandle))

    [<Test>]
    let ``PerInstInfo projection refuses non-generic concrete types`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                projectFromState loggerFactory (stateWithLogger loggerFactory) "PerInstInfo" (handleFor bct.Int32)
                |> ignore
            )

        ex.Message |> shouldContainText "PerInstInfo"
        ex.Message |> shouldContainText "Nullable"

    [<Test>]
    let ``PerInstInfo projection refuses non-Nullable closed generics`` () : unit =
        // List<int> has a single dictionary (no generic ancestors) so in
        // principle the first PerInstInfo slot would hold int's MethodTable,
        // but PawPrint only commits to the Nullable layout today. The
        // classifier must refuse this case to keep its contract truthful;
        // broadening requires explicit dictionary-index modelling for types
        // whose inheritance chain contributes additional dictionaries.
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let intHandle = handleFor bct.Int32

        let state, listIntHandle =
            topLevelType "System.Collections.Generic" "List`1"
            |> DumpedAssembly.typeInfoToTypeDefn' bct (stateWithLogger loggerFactory)._LoadedAssemblies
            |> IlMachineState.concretizeType
                loggerFactory
                bct
                (stateWithLogger loggerFactory)
                corelib.Name
                (ImmutableArray.Create intHandle)
                ImmutableArray.Empty

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                projectFromState loggerFactory state "PerInstInfo" listIntHandle |> ignore
            )

        ex.Message |> shouldContainText "PerInstInfo"
        ex.Message |> shouldContainText "Nullable"

    [<Test>]
    let ``PerInstInfo projection refuses array handles`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                projectFromState loggerFactory (stateWithLogger loggerFactory) "PerInstInfo" intArrayHandle
                |> ignore
            )

        ex.Message |> shouldContainText "PerInstInfo"
        ex.Message |> shouldContainText "array"

    [<Test>]
    let ``PerInstInfo projection refuses open generic definitions`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let target =
            topLevelType "System" "Nullable`1"
            |> _.Identity
            |> RuntimeTypeHandleTarget.OpenGenericTypeDefinition

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                MethodTableProjection.tryProjectFieldForRuntimeTypeHandleTarget
                    loggerFactory
                    bct
                    (methodTableField "PerInstInfo")
                    target
                    (stateWithLogger loggerFactory)
                |> ignore
            )

        ex.Message |> shouldContainText "PerInstInfo"
        ex.Message |> shouldContainText "open generic"

    [<Test>]
    let ``Ldfld projects PerInstInfo from MethodTable pointer for closed generic`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let field = methodTableField "PerInstInfo"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let intHandle = handleFor bct.Int32

        let state, nullableIntHandle =
            concretizeNullableOf loggerFactory intHandle (stateWithLogger loggerFactory)

        let state, thread = stateWithSingleInstructionFromState loggerFactory op state

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed nullableIntHandle)
                ))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.NativeInt (NativeIntSource.PerInstInfoPtr nullableIntHandle)))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldind_i on PerInstInfoPtr steps to PerInstDictPtr for same handle`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let op = IlOp.Nullary NullaryIlOp.Ldind_i
        let intHandle = handleFor bct.Int32

        let state, nullableIntHandle =
            concretizeNullableOf loggerFactory intHandle (stateWithLogger loggerFactory)

        let state, thread = stateWithSingleInstructionFromState loggerFactory op state

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.PerInstInfoPtr nullableIntHandle))
                thread

        let state =
            match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Ldind_i with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected stepped execution, got %O{other}"

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.NativeInt (NativeIntSource.PerInstDictPtr nullableIntHandle)))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldind_i on PerInstDictPtr resolves first generic argument's MethodTable`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let op = IlOp.Nullary NullaryIlOp.Ldind_i
        let intHandle = handleFor bct.Int32

        let state, nullableIntHandle =
            concretizeNullableOf loggerFactory intHandle (stateWithLogger loggerFactory)

        let state, thread = stateWithSingleInstructionFromState loggerFactory op state

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.PerInstDictPtr nullableIntHandle))
                thread

        let state =
            match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Ldind_i with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected stepped execution, got %O{other}"

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (
            Some (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed intHandle)))
        )

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldfld projects MethodTableAuxiliaryData flags from auxiliary-data pointer provenance`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let field = methodTableAuxiliaryDataField "Flags"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let intHandle = handleFor bct.Int32

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (
                    NativeIntSource.MethodTableAuxiliaryDataPtr (RuntimeTypeHandleTarget.Closed intHandle)
                ))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 0))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldfld projects RawArrayData length from structured array storage`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = rawArrayDataField "Length"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let arrayAddr, state = allocateIntArray 3 state

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef arrayAddr) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 3))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldflda projects RawArrayData data as a byte view of array storage`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = rawArrayDataField "Data"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let arrayAddr, state = allocateIntArray 3 state

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef arrayAddr) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldflda token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actualArrayAddr,
                                                                                                   actualIndex),
                                                                           [ ByrefProjection.ReinterpretAs actualView ]))) ->
            actualArrayAddr |> shouldEqual arrayAddr
            actualIndex |> shouldEqual 0
            actualView |> shouldEqual (concreteTypeFor bct.Byte)
        | other -> failwith $"Expected RawArrayData::Data byte-view byref, got %O{other}"

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``Ldflda projects RawData data as a byte view of boxed value storage`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let field = rawDataField "Data"
        let token = MetadataToken.FieldDefinition field.Handle
        let token = SourcedMetadataToken.make corelib.Name token
        let op = IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, token)
        let state, thread = stateWithSingleInstruction loggerFactory op
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef boxedAddr) thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldflda token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        match IlMachineState.peekEvalStack thread state with
        | Some (EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.HeapValue actualAddr,
                                                                           [ ByrefProjection.ReinterpretAs actualView ]))) ->
            actualAddr |> shouldEqual boxedAddr
            actualView |> shouldEqual (concreteTypeFor bct.Byte)
        | other -> failwith $"Expected RawData::Data boxed-value byte-view byref, got %O{other}"

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``RawData boxed value byte view writes back into boxed storage`` () : unit =
        let initialBytes =
            [| 0x08uy ; 0x07uy ; 0x06uy ; 0x05uy ; 0x04uy ; 0x03uy ; 0x02uy ; 0x01uy |]

        let expectedBytes =
            [| 0x08uy ; 0x07uy ; 0xEFuy ; 0xBEuy ; 0x04uy ; 0x03uy ; 0x02uy ; 0x01uy |]

        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state
        let rawDataPtr = projectRawDataDataPointer boxedAddr state

        boxedPayloadBytes boxedAddr state |> shouldEqual initialBytes

        let ptrAtOffset =
            rawDataPtr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 2)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                ptrAtOffset
                (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))

        let updated =
            ManagedHeap.get boxedAddr state.ManagedHeap
            |> _.Contents
            |> CliValueType.ToBytes

        updated |> shouldEqual expectedBytes

        IlMachineState.readManagedByrefBytesAs bct state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))

    [<Test>]
    let ``RawData boxed value byte view reads original boxed storage`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state

        let ptrAtOffset =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 2)

        IlMachineState.readManagedByrefBytesAs bct state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 0x0506us))

    [<Test>]
    let ``RawData boxed value byte view round-trips UInt16 writes`` () : unit =
        let property (sample : RawDataWriteCase) : unit =
            let state = state ()
            let boxedAddr, state = allocateBoxedIntPtr sample.Initial state

            let ptrAtOffset =
                projectRawDataDataPointer boxedAddr state
                |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset sample.Offset)

            let expectedBytes = System.BitConverter.GetBytes sample.Initial
            let payloadBytes = System.BitConverter.GetBytes sample.Payload
            Array.blit payloadBytes 0 expectedBytes sample.Offset payloadBytes.Length

            let state =
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    ptrAtOffset
                    (CliType.Numeric (CliNumericType.UInt16 sample.Payload))

            boxedPayloadBytes boxedAddr state |> shouldEqual expectedBytes

            IlMachineState.readManagedByrefBytesAs bct state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
            |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 sample.Payload))

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genRawDataWriteCase) property)

    [<Test>]
    let ``RawData boxed value byte-identical writes preserve boxed payload identity`` () : unit =
        let property (sample : ByteIdenticalUInt16WriteCase) : unit =
            let state = state ()
            let boxedAddr, state = allocateBoxedIntPtr sample.Initial state
            let payloadBefore = boxedPayloadValueType boxedAddr state
            let initialBytes = System.BitConverter.GetBytes sample.Initial
            let payload = System.BitConverter.ToUInt16 (initialBytes, sample.Offset)

            let ptr =
                projectRawDataDataPointer boxedAddr state
                |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset sample.Offset)

            let state =
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    ptr
                    (CliType.Numeric (CliNumericType.UInt16 payload))

            let payloadAfter = boxedPayloadValueType boxedAddr state

            System.Object.ReferenceEquals (payloadAfter, payloadBefore) |> shouldEqual true

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genByteIdenticalUInt16WriteCase) property)

    [<Test>]
    let ``Reinterpreted struct field writes update the underlying storage bytes`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory

        let property (sample : ReinterpretByteWriteCase) : unit =
            for fieldIndex = 0 to 3 do
                let addr, state = allocateInt32Wrapper types sample.Initial

                let state =
                    writeFourBytesField types fieldIndex sample.Payloads.[fieldIndex] addr state

                let expectedBytes = System.BitConverter.GetBytes sample.Initial
                expectedBytes.[fieldIndex] <- sample.Payloads.[fieldIndex]

                boxedPayloadBytes addr state |> shouldEqual expectedBytes

                wrapperValue types addr state
                |> shouldEqual (System.BitConverter.ToInt32 (expectedBytes, 0))

        Check.One (reinterpretWritePropertyConfig, Prop.forAll (Arb.fromGen genReinterpretByteWriteCase) property)

    [<Test>]
    let ``Reinterpreted struct byte-identical field writes preserve state identity`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let initial = 0x01020304
        let initialBytes = System.BitConverter.GetBytes initial

        for fieldIndex = 0 to 3 do
            let addr, state = allocateInt32Wrapper types initial

            let stateAfter =
                writeFourBytesField types fieldIndex initialBytes.[fieldIndex] addr state

            System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true

    [<Test>]
    let ``Nested reinterpreted struct field write recurses through inner view`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let addr, state = allocateInt32Wrapper types 0x01020304

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [
                    ByrefProjection.ReinterpretAs types.FourByteWrappersConcrete
                    ByrefProjection.Field types.FourByteWrapperFields.[0]
                    ByrefProjection.ReinterpretAs types.ByteWrapperConcrete
                    ByrefProjection.Field types.ByteWrapperValueField
                ]
            )

        let state =
            IlMachineState.writeManagedByrefWithBase bct state ptr (CliType.Numeric (CliNumericType.UInt8 0xFFuy))

        wrapperValue types addr state |> shouldEqual 0x010203FF

    [<Test>]
    let ``Reinterpreted byte-offset write recurses through inner view`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let addr, state = allocateInt32Wrapper types 0x01020304

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [
                    ByrefProjection.ReinterpretAs types.FourBytesConcrete
                    ByrefProjection.ByteOffset 1
                    ByrefProjection.ReinterpretAs types.ByteWrapperConcrete
                    ByrefProjection.Field types.ByteWrapperValueField
                ]
            )

        let state =
            IlMachineState.writeManagedByrefWithBase bct state ptr (CliType.Numeric (CliNumericType.UInt8 0xEEuy))

        wrapperValue types addr state |> shouldEqual 0x0102EE04

    [<Test>]
    let ``Reinterpreted read of runtime-pointer field reports unsupported byte view`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let addr, state = allocateInt32Wrapper types 0x01020304

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapValue addr,
                [ ByrefProjection.ReinterpretAs types.PointerWrapperConcrete ]
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefField bct state ptr types.PointerWrapperPtrField
                |> ignore
            )

        ex.Message |> shouldContainText "runtime-pointer field"
        ex.Message |> shouldContainText "pointer byte views are not modelled"

    [<Test>]
    let ``Reinterpreted write over object-reference storage reports unsupported storage shape`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let state = types.State
        let objectAddr, state = allocateReferenceObject state
        let objectHandle = handleFor bct.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef (Some objectAddr))
                1
                state

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arrayAddr, 0),
                [
                    ByrefProjection.ReinterpretAs types.FourBytesConcrete
                    ByrefProjection.Field types.FourBytesFields.[0]
                ]
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefWithBase bct state ptr (CliType.Numeric (CliNumericType.UInt8 0xFFuy))
                |> ignore
            )

        // The iterative byte-view peel collapses `[ReinterpretAs FourBytes, Field _]` over an
        // `object[]` element to a byte write at offset 0 of the element. The array-element byte
        // writer then refuses the byte view because the element holds an object reference whose
        // bytes are not part of the model. We still get a clear failure attributing to the storage
        // shape; the message just no longer attributes through `ReinterpretAs` because the peel
        // produced a residual offset-only chain, not a residual reinterpret.
        ex.Message |> shouldContainText "refusing byte view over object reference"

        ex.Message
        |> shouldContainText "byte-addressability: rejected: object reference"

        ex.Message |> shouldContainText "Value layout:"

    [<Test>]
    let ``Reinterpreted write over runtime-pointer value-type storage reports unsupported storage shape`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory
        let state = types.State
        let valueType = runtimePointerValueType state
        let arrayAddr, state = allocateSingleValueTypeArray valueType state

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arrayAddr, 0),
                [
                    ByrefProjection.ReinterpretAs types.FourBytesConcrete
                    ByrefProjection.Field types.FourBytesFields.[0]
                ]
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefWithBase bct state ptr (CliType.Numeric (CliNumericType.UInt8 0xFFuy))
                |> ignore
            )

        // The iterative byte-view peel collapses `[ReinterpretAs FourBytes, Field _]` over an
        // array element whose declared type is a runtime-pointer-bearing value type to a byte
        // write at offset 0 of the element. The array-element byte writer refuses because the
        // element's value type contains runtime pointers whose bytes are not part of the model.
        // We still get a clear failure attributing to the storage shape.
        ex.Message
        |> shouldContainText "refusing byte view over value type containing runtime pointers"

        ex.Message |> shouldContainText "Value layout:"

        ex.Message
        |> shouldContainText "byte-addressability: rejected: value type containing runtime pointers"

    [<Test>]
    let ``Bare boxed value byref byte view round-trips through boxed storage`` () : unit =
        let initialBytes =
            [| 0x08uy ; 0x07uy ; 0x06uy ; 0x05uy ; 0x04uy ; 0x03uy ; 0x02uy ; 0x01uy |]

        let expectedBytes =
            [| 0xDDuy ; 0xCCuy ; 0xBBuy ; 0xAAuy ; 0x04uy ; 0x03uy ; 0x02uy ; 0x01uy |]

        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.HeapValue boxedAddr, [])

        let replacement =
            System.BitConverter.ToInt32 ([| 0xDDuy ; 0xCCuy ; 0xBBuy ; 0xAAuy |], 0)

        boxedPayloadBytes boxedAddr state |> shouldEqual initialBytes

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.UInt16 0us))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 0x0708us))

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                ptr
                (CliType.Numeric (CliNumericType.Int32 replacement))

        boxedPayloadBytes boxedAddr state |> shouldEqual expectedBytes

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 replacement))

    [<Test>]
    let ``Array value-type byte-identical writes preserve element payload identity`` () : unit =
        let property (sample : ByteIdenticalUInt16WriteCase) : unit =
            let state = state ()
            let boxedAddr, state = allocateBoxedIntPtr sample.Initial state
            let valueType = boxedPayloadValueType boxedAddr state
            let arrayAddr, state = allocateSingleValueTypeArray valueType state
            let arrayBefore = state.ManagedHeap.Arrays.[arrayAddr]
            let payloadBefore = arrayElementValueType arrayAddr 0 state
            let initialBytes = System.BitConverter.GetBytes sample.Initial
            let payload = System.BitConverter.ToUInt16 (initialBytes, sample.Offset)

            let ptr =
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arrayAddr, 0),
                    [
                        ByrefProjection.ReinterpretAs (concreteTypeFor bct.Byte)
                        ByrefProjection.ByteOffset sample.Offset
                    ]
                )

            let state =
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    ptr
                    (CliType.Numeric (CliNumericType.UInt16 payload))

            let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]
            let payloadAfter = arrayElementValueType arrayAddr 0 state

            System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true
            System.Object.ReferenceEquals (payloadAfter, payloadBefore) |> shouldEqual true

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genByteIdenticalUInt16WriteCase) property)

    [<Test>]
    let ``Value-type field byte-identical byref writes preserve state identity`` () : unit =
        let property (sample : ByteIdenticalFieldWriteCase) : unit =
            let state = state ()
            let boxedAddr, state = allocateBoxedIntPtr sample.Initial state
            let valueType = boxedPayloadValueType boxedAddr state
            let arrayAddr, state = allocateSingleValueTypeArray valueType state
            let valueField = intPtrValueFieldId ()
            let initialBytes = System.BitConverter.GetBytes sample.Initial

            let plainPtr =
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [ ByrefProjection.Field valueField ])

            let plainPayload = System.BitConverter.ToUInt16 (initialBytes, 0)

            let stateAfterPlain =
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    plainPtr
                    (CliType.Numeric (CliNumericType.UInt16 plainPayload))

            System.Object.ReferenceEquals (stateAfterPlain, state) |> shouldEqual true

            let byteViewPtr =
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arrayAddr, 0),
                    [
                        ByrefProjection.Field valueField
                        ByrefProjection.ReinterpretAs (concreteTypeFor bct.Byte)
                        ByrefProjection.ByteOffset sample.Offset
                    ]
                )

            let stateAfterByteView =
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    byteViewPtr
                    (CliType.Numeric (CliNumericType.UInt8 initialBytes.[sample.Offset]))

            System.Object.ReferenceEquals (stateAfterByteView, state) |> shouldEqual true

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genByteIdenticalFieldWriteCase) property)

    [<Test>]
    let ``Array primitive NaN byte-identical write preserves array identity`` () : unit =
        let state = state ()
        let nan = CliType.Numeric (CliNumericType.Float64 System.Double.NaN)
        let doubleArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Double)

        let arrayAddr, state =
            IlMachineState.allocateArray doubleArrayHandle (fun () -> nan) 1 state

        let arrayBefore = state.ManagedHeap.Arrays.[arrayAddr]
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr nan
        let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]

        System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true

    [<Test>]
    let ``Array primitive reinterpret byte-identical write preserves array identity`` () : unit =
        let state = state ()
        let initial = CliType.Numeric (CliNumericType.UInt16 0xAAFFus)
        let shortWithSameBytes = CliType.Numeric (CliNumericType.Int16 -21761s)
        let ushortArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.UInt16)

        let arrayAddr, state =
            IlMachineState.allocateArray ushortArrayHandle (fun () -> initial) 1 state

        let arrayBefore = state.ManagedHeap.Arrays.[arrayAddr]

        let ptr =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arrayAddr, 0),
                [ ByrefProjection.ReinterpretAs (concreteTypeFor bct.Int16) ]
            )

        let state =
            IlMachineState.writeManagedByrefWithBase bct state ptr shortWithSameBytes

        let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]

        System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true
        IlMachineState.getArrayValue arrayAddr 0 state |> shouldEqual initial

    [<Test>]
    let ``Array primitive byte-identical write spanning cells preserves array identity`` () : unit =
        let state = state ()
        let arrayAddr, state = allocateIntArray 2 state

        let first = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let second = CliType.Numeric (CliNumericType.Int32 0x55667788)

        let state =
            state
            |> IlMachineState.setArrayValue arrayAddr first 0
            |> IlMachineState.setArrayValue arrayAddr second 1

        let arrayBefore = state.ManagedHeap.Arrays.[arrayAddr]

        let writtenBytes =
            [| yield! CliType.ToBytes first ; yield! CliType.ToBytes second |]

        let written =
            CliType.Numeric (
                CliNumericType.Int64 (Int64Source.Verbatim (System.BitConverter.ToInt64 (writtenBytes, 0)))
            )

        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])
        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr written
        let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]

        System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true
        IlMachineState.getArrayValue arrayAddr 0 state |> shouldEqual first
        IlMachineState.getArrayValue arrayAddr 1 state |> shouldEqual second

    [<Test>]
    let ``Array primitive signed-zero byte writes preserve written bytes`` () : unit =
        let observed = HashSet<bool * bool> ()

        let property (sample : SignedZeroWriteCase) : unit =
            observed.Add ((sample.InitialNegative, sample.WrittenNegative)) |> ignore

            let state = state ()

            let initial =
                CliType.Numeric (CliNumericType.Float64 (signedZero sample.InitialNegative))

            let written =
                CliType.Numeric (CliNumericType.Float64 (signedZero sample.WrittenNegative))

            let doubleArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Double)

            let arrayAddr, state =
                IlMachineState.allocateArray doubleArrayHandle (fun () -> initial) 1 state

            let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])
            let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr written

            let actual =
                IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.Float64 0.0))

            CliType.ToBytes actual |> shouldEqual (CliType.ToBytes written)

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genSignedZeroWriteCase) property)

        for initialNegative in [ false ; true ] do
            for writtenNegative in [ false ; true ] do
                observed.Contains ((initialNegative, writtenNegative)) |> shouldEqual true

    [<Test>]
    let ``String byte-identical byte-view writes preserve state identity`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = stateWithLogger loggerFactory

        let stringAddr, state =
            IlMachineState.allocateManagedString loggerFactory bct "AZ" state

        let ptr = ManagedPointerSource.Byref (ByrefRoot.StringCharAt (stringAddr, 0), [])

        let stateAfter =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr (CliType.ofChar 'A')

        System.Object.ReferenceEquals (stateAfter, state) |> shouldEqual true

    [<Test>]
    let ``Local memory byte-identical writes preserve state identity`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let frame = state.ThreadState.[thread].ActiveMethodState

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let block =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (_, _, block, 0), []) -> block
            | other -> failwith $"Expected local-memory root pointer, got %O{other}"

        let initial = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr initial

        let bareBytePtr =
            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, 0), [])

        let stateAfterBare =
            IlMachineState.writeManagedByref state bareBytePtr (CliType.Numeric (CliNumericType.UInt8 0x44uy))

        System.Object.ReferenceEquals (stateAfterBare, state) |> shouldEqual true

        let stateAfterWide =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr initial

        System.Object.ReferenceEquals (stateAfterWide, state) |> shouldEqual true

    [<Test>]
    let ``Local memory same-size byte-identical writes restamp primitive shape`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let state =
            IlMachineState.writeManagedByref state ptr (CliType.Numeric (CliNumericType.Int32 0))

        let updated = CliType.Numeric (CliNumericType.Float32 0.0f)
        let state = IlMachineState.writeManagedByref state ptr updated

        IlMachineState.readManagedByref bct state ptr |> shouldEqual updated

    [<Test>]
    let ``Local memory typed cell write evicts intersecting byte overlay`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let frame = state.ThreadState.[thread].ActiveMethodState

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let block =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (_, _, block, 0), []) -> block
            | other -> failwith $"Expected local-memory root pointer, got %O{other}"

        let byteViewAt (offset : int) : ManagedPointerSource =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs (concreteTypeFor bct.Byte))
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset offset)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                (byteViewAt 1)
                (CliType.Numeric (CliNumericType.UInt8 0xAAuy))

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                (byteViewAt 2)
                (CliType.Numeric (CliNumericType.UInt8 0xBBuy))

        let pool = IlMachineState.getStackMemoryPool thread frame state
        let blockBeforeTypedWrite = StackMemoryPool.getBlock block pool
        Map.count blockBeforeTypedWrite.Bytes |> shouldEqual 2

        let updated = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr updated

        let pool = IlMachineState.getStackMemoryPool thread frame state
        let blockAfterTypedWrite = StackMemoryPool.getBlock block pool

        Map.isEmpty blockAfterTypedWrite.Bytes |> shouldEqual true
        blockAfterTypedWrite.Cells |> Map.tryFind 0 |> shouldEqual (Some updated)

    [<Test>]
    let ``Reinterpret read of same-width primitive cells reconstructs the requested shape`` () : unit =
        // Regression: the typed-cell fast path used to return the underlying
        // cell when the requested template had the same size, which bypassed
        // the bit reinterpret. Reading via `readManagedByrefBytesAs` should
        // reconstruct the requested primitive from the cell's bytes.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let int32ToFloat32Ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        // 0x40490FDB is the IEEE-754 bit pattern for ~3.14159f.
        let intInitial = CliType.Numeric (CliNumericType.Int32 0x40490FDB)
        let state = IlMachineState.writeManagedByref state int32ToFloat32Ptr intInitial

        let actual =
            IlMachineState.readManagedByrefBytesAs
                bct
                state
                int32ToFloat32Ptr
                (CliType.Numeric (CliNumericType.Float32 0.0f))

        match actual with
        | CliType.Numeric (CliNumericType.Float32 f) ->
            // pi as float32 lies between 3.14159 and 3.1416; assert a tight band.
            (f > 3.14f && f < 3.15f) |> shouldEqual true
        | other -> failwith $"Expected Float32, got %O{other}"

        let float32ToInt32Ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let float32Initial = 3.1415927f

        let state =
            IlMachineState.writeManagedByref
                state
                float32ToInt32Ptr
                (CliType.Numeric (CliNumericType.Float32 float32Initial))

        IlMachineState.readManagedByrefBytesAs bct state float32ToInt32Ptr (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 (System.BitConverter.SingleToInt32Bits float32Initial)))

        let int64ToFloat64Ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        // 0x400921FB54442D18 is the IEEE-754 bit pattern for Math.PI.
        let int64Initial =
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0x400921FB54442D18L))

        let state = IlMachineState.writeManagedByref state int64ToFloat64Ptr int64Initial

        let actual =
            IlMachineState.readManagedByrefBytesAs
                bct
                state
                int64ToFloat64Ptr
                (CliType.Numeric (CliNumericType.Float64 0.0))

        match actual with
        | CliType.Numeric (CliNumericType.Float64 f) -> (f > 3.14 && f < 3.15) |> shouldEqual true
        | other -> failwith $"Expected Float64, got %O{other}"

        let float64ToInt64Ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let float64Initial = System.Math.PI

        let state =
            IlMachineState.writeManagedByref
                state
                float64ToInt64Ptr
                (CliType.Numeric (CliNumericType.Float64 float64Initial))

        IlMachineState.readManagedByrefBytesAs
            bct
            state
            float64ToInt64Ptr
            (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
        |> shouldEqual (
            CliType.Numeric (
                CliNumericType.Int64 (Int64Source.Verbatim (System.BitConverter.DoubleToInt64Bits float64Initial))
            )
        )

    [<Test>]
    let ``Writing a tagged native-int through a StackMemoryByte byref preserves provenance`` () : unit =
        // Regression: the noop check used to call `CliType.ToBytes` on the
        // value being written, which throws for tagged NativeInt sources such
        // as `FieldHandlePtr`. Writing a `FieldHandlePtr` through a bare
        // StackMemoryByte byref should succeed and round-trip the provenance.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let handle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let stateAfter = IlMachineState.writeManagedByref state ptr handle

        IlMachineState.readManagedByref bct stateAfter ptr |> shouldEqual handle

    [<Test>]
    let ``Typed write into the middle of an existing cell fails visibly`` () : unit =
        // Regression: the writeRootValue StackMemoryByte arm used to call
        // `writeCell` directly, which silently evicted any covering cell —
        // including a tagged-pointer cell whose provenance would be lost. The
        // read-side already failed in this case; the write side now mirrors
        // that.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let frame = state.ThreadState.[thread].ActiveMethodState

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let block =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (_, _, block, 0), []) -> block
            | other -> failwith $"Expected local-memory root pointer, got %O{other}"

        let cell = CliType.Numeric (CliNumericType.Int32 0x11223344)
        let state = IlMachineState.writeManagedByref state ptr cell

        let midCellPtr =
            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, 1), [])

        Assert.Throws<System.Exception> (fun () ->
            IlMachineState.writeManagedByref state midCellPtr (CliType.Numeric (CliNumericType.UInt8 0xFFuy))
            |> ignore
        )
        |> ignore

    [<Test>]
    let ``Stind-shaped store of tagged native-int through bare StackMemoryByte preserves provenance`` () : unit =
        // Regression: `Localloc` pushes its result as
        // `EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)` (see
        // NullaryIlOp.fs:1543-1544), so `Stind` dispatches the store through
        // `writeManagedByrefBytesOrTypedCell` rather than the typed
        // `writeManagedByrefWithBase`. Without provenance preservation in the
        // bytes path, `CliType.ToBytes` throws on tagged NativeInt sources
        // such as `FieldHandlePtr`, and even byte-flattenable values would
        // lose their typed cell. The bytes path must short-circuit to a
        // typed-cell write for bare `StackMemoryByte` byrefs so the
        // stackalloc-then-stind pattern matches the typed-byref store.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let handle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 9876L))

        let stateAfter =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr handle

        IlMachineState.readManagedByref bct stateAfter ptr |> shouldEqual handle

    [<Test>]
    let ``Stind-shaped store of tagged native-int over an identical-shape cell still preserves provenance`` () : unit =
        // Even when an existing cell already lives at the same offset, the
        // typed-cell fast path must still preserve the new value's
        // provenance (and survive the round trip) provided the existing
        // cell has the same size — replacing in place produces equivalent
        // bytes, so it is observably equivalent to byte scatter while
        // preserving the new tag.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let firstHandle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1L))

        let secondHandle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 2L))

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr firstHandle

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr secondHandle

        IlMachineState.readManagedByref bct state ptr |> shouldEqual secondHandle

    [<Test>]
    let ``Stind-shaped partial overwrite of a tagged native-int cell refuses to silently lose provenance`` () : unit =
        // A 4-byte store at offset 0 of an 8-byte tagged-pointer cell must
        // NOT silently evict the tagged cell (which would lose the
        // unwritten high half's provenance). The strict typed-write guard
        // refuses the fast path when sizes differ; the byte scatter path
        // then throws because tagged-pointer cells are not byte-addressable.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let handle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 0xDEADL))

        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr handle

        Assert.Throws<System.Exception> (fun () ->
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr (CliType.Numeric (CliNumericType.Int32 42))
            |> ignore
        )
        |> ignore

        // The original tagged cell should still be intact.
        IlMachineState.readManagedByref bct state ptr |> shouldEqual handle

    [<Test>]
    let ``Stind_I through bare local-memory byte view reports provenance preservation failure`` () : unit =
        // When the destination shape is not a whole-cell replacement, a
        // provenance-bearing primitive payload cannot be scattered as bytes.
        // The primitive stind dispatcher should report that contract directly
        // instead of leaking the lower-level byte-rendering failure.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let state =
            IlMachineState.writeManagedByref state ptr (CliType.Numeric (CliNumericType.Int32 0x11223344))

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                let state =
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                    |> IlMachineState.pushToEvalStack'
                        (EvalStackValue.NativeInt (NativeIntSource.FieldHandlePtr 1234L))
                        thread

                NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I |> ignore
            )

        ex.Message |> shouldContainText "primitive indirect store"

        ex.Message
        |> shouldContainText "cannot preserve new value's native int with non-byte-addressable provenance"

        ex.Message |> shouldContainText "<field ID 1234>"

        IlMachineState.readManagedByref bct state ptr
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x11223344))

    [<Test>]
    let ``Stind-shaped byte write that lands inside an existing cell still flattens through bytes`` () : unit =
        // Even with the provenance-preserving fast path for bare
        // `StackMemoryByte` byrefs, byte-aligned writes that fall inside
        // (rather than at the start of) an existing cell must still go
        // through the byte overlay so partial-cell `stind.i1` etc. continue
        // to work. We install an Int32 cell at offset 0 and then write a
        // single UInt8 at offset 1, which should overlay the second byte of
        // the Int32 in little-endian order.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let frame = state.ThreadState.[thread].ActiveMethodState

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let block =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (_, _, block, 0), []) -> block
            | other -> failwith $"Expected local-memory root pointer, got %O{other}"

        // Install a typed Int32 cell at offset 0.
        let state =
            IlMachineState.writeManagedByref state ptr (CliType.Numeric (CliNumericType.Int32 0x11223344))

        // Write a UInt8 at offset 1 via the bytes path. This is the dispatch
        // shape that an unaligned stackalloc store would produce.
        let midCellPtr =
            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, 1), [])

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                midCellPtr
                (CliType.Numeric (CliNumericType.UInt8 0xAAuy))

        // Reading the Int32 cell should reflect the byte overlay: the second
        // little-endian byte of 0x11223344 (0x33) becomes 0xAA, giving
        // 0x1122AA44.
        match IlMachineState.readManagedByref bct state ptr with
        | CliType.Numeric (CliNumericType.Int32 v) -> v |> shouldEqual 0x1122AA44
        | other -> failwith $"Expected Int32, got %O{other}"

    [<Test>]
    let ``Stind-shaped fresh write of byte-equivalent zero installs a typed cell`` () : unit =
        // Regression for the noop-check in `writeRootValue`'s StackMemoryByte
        // arm: when the destination has no existing cell and the freshly
        // installed value's bytes happen to equal what a byte read would
        // return (e.g. Int32 0 over zero-initialised memory), the noop check
        // must NOT short-circuit — there is still no typed cell, and a later
        // bare typed read would fail. The fix is to only treat the write as a
        // noop when a cell already lives at the destination.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let zero = CliType.Numeric (CliNumericType.Int32 0)

        let stateAfter = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr zero

        IlMachineState.readManagedByref bct stateAfter ptr |> shouldEqual zero

    [<Test>]
    let ``Stind-shaped overwrite of a tagged cell with a byte-renderable value succeeds`` () : unit =
        // Regression for `StackMemoryPool.tryReadBytes` learning from
        // `ByteAddressability` that the existing cell carries unrenderable
        // provenance (e.g. a `FieldHandlePtr`-tagged native int). The
        // noop-check shortcut must return `ValueNone` and fall through to the
        // typed write instead of trying to flatten the tagged native int.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let handle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr handle

        let verbatim =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 5678L))

        let state = IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr verbatim

        IlMachineState.readManagedByref bct state ptr |> shouldEqual verbatim

    [<Test>]
    let ``Stind_I1 via EvalStackValue.ManagedPointer over local-memory Int32 cell scatters one byte`` () : unit =
        // Regression: a `ManagedPointer`-shaped stind (e.g. when a localloc
        // pointer has been stashed in a managed-pointer-typed slot and
        // reloaded) currently routes through `writeManagedByrefWithBase`,
        // which dispatches a bare `StackMemoryByte` byref straight to
        // `writeRootValue` and installs the new value as a typed cell. That
        // evicts the existing Int32 cell, so `stind.i1 0xAA` over Int32
        // 0x11223344 leaves only a one-byte cell behind: a later byte-view
        // ldind.i4 sees 0x000000AA instead of 0x112233AA.
        //
        // CIL III.4.27 is unambiguous: `stind.i1` writes one byte at the
        // pointed-to location. Local-memory and other byref destinations
        // must agree on byte-scatter semantics for the partial primitive
        // stind.* opcodes; the typed-cell store is a separate operation.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I1)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        // Install the wider Int32 cell that the partial stind should preserve.
        let state =
            IlMachineState.writeManagedByref state ptr (CliType.Numeric (CliNumericType.Int32 0x11223344))

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0xAA) thread

        let state =
            match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I1 with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected Stind_I1 to step, got %O{other}"

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x112233AA))

    [<Test>]
    let ``Stind_I1 via EvalStackValue.ManagedPointer over an Int32 array element scatters one byte`` () : unit =
        // Sibling of the local-memory regression: the same partial-stind
        // dispatch over a managed-storage byref must update only the
        // addressed byte of the existing element, leaving the element's
        // declared Int32 shape intact. The managed-array path goes through
        // `writeManagedByrefBytesOrTypedCell`, which rebuilds from the existing Int32
        // cell template after applying the one-byte overwrite.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I1)

        let arrayAddr, state = allocateIntArray 1 state

        let state =
            IlMachineState.setArrayValue arrayAddr (CliType.Numeric (CliNumericType.Int32 0x11223344)) 0 state

        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0xAA) thread

        let state =
            match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I1 with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected Stind_I1 to step, got %O{other}"

        IlMachineState.getArrayValue arrayAddr 0 state
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x112233AA))

    [<Test>]
    let ``Stind_I via EvalStackValue.ManagedPointer over a tagged native-int cell preserves provenance`` () : unit =
        // Provenance guard: a full-width primitive stind.i through a
        // `ManagedPointer`-shaped byref into a same-sized native-int cell
        // must preserve the new value's tagged provenance (FieldHandlePtr,
        // MethodTablePtr, ...). Byte scatter would fail because such cells
        // are not byte-addressable; the typed-cell fast path stays valid
        // because the new cell exactly replaces the old at the same offset
        // and size.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let firstHandle =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr 1234L))

        let state = IlMachineState.writeManagedByref state ptr firstHandle

        let secondHandle = NativeIntSource.FieldHandlePtr 5678L

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt secondHandle) thread

        let state =
            match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected Stind_I to step, got %O{other}"

        IlMachineState.readManagedByref bct state ptr
        |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt secondHandle))

    [<Test>]
    let ``Stind_I preserves tagged native-int provenance for exact-width typed destinations`` () : unit =
        // Generated version of the provenance guard: for every destination
        // shape where the existing typed slot is native-int-sized, stind.i of
        // a tagged NativeIntSource must keep the tag instead of trying to
        // flatten it to bytes.
        let observedSources = HashSet<NativeIntSource> ()
        let observedDestinations = HashSet<TaggedNativeIntDestination> ()

        let property (source : NativeIntSource, destination : TaggedNativeIntDestination) : unit =
            observedSources.Add source |> ignore
            observedDestinations.Add destination |> ignore

            let _, loggerFactory = LoggerFactory.makeTest ()

            let state, thread =
                stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I)

            let initial =
                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

            let ptr, state =
                match destination with
                | TaggedNativeIntDestination.StackMemory ->
                    let ptr, state =
                        IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

                    ptr, IlMachineState.writeManagedByref state ptr initial
                | TaggedNativeIntDestination.NativeIntArrayElement ->
                    let nativeIntArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.IntPtr)

                    let arrayAddr, state =
                        IlMachineState.allocateArray nativeIntArrayHandle (fun () -> initial) 1 state

                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state
                | TaggedNativeIntDestination.IntPtrField ->
                    let boxedAddr, state = allocateBoxedIntPtr 0L state

                    ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (boxedAddr, intPtrValueFieldId ()), []), state

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt source) thread

            let state =
                match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I with
                | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
                | other -> failwith $"Expected Stind_I to step, got %O{other}"

            IlMachineState.readManagedByref bct state ptr
            |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt source))

        Check.One (
            rawDataPropertyConfig.WithMaxTest 500,
            Prop.forAll (Arb.fromGen genTaggedNativeIntStindCase) property
        )

        // The expected-source helper mints a fresh MethodInfo for the
        // FunctionPointer case each time. The count assertion is the coverage
        // check; it intentionally does not compare list membership.
        observedSources.Count |> shouldEqual ((taggedNativeIntSources ()).Length)
        observedDestinations.Count |> shouldEqual 3

    [<Test>]
    let ``readManagedByref through ReinterpretAs IntPtr preserves tagged native-int provenance`` () : unit =
        // Regression: when the byref carries a trailing `ReinterpretAs IntPtr`
        // projection (the shape produced by the `Span<IntPtr>(void*, int)`
        // constructor over a `stackalloc` buffer) and the storage cell holds a
        // tagged `NativeIntSource` (e.g. `TypeHandlePtr` written by
        // `Stind_I`), `readManagedByref` must return the bare native-int cell
        // with its provenance intact. The byte-view fallback in
        // `readStackMemoryBytesAs` cannot serialise tagged sources, so the
        // fast path that returns the existing typed cell is load-bearing for
        // the `RuntimeTypeHandle.Instantiate` / `ModuleHandle.ResolveType`
        // QCalls that walk such buffers.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let bareLocallocPtr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let taggedHandle =
            CliType.Numeric (
                CliNumericType.NativeInt (
                    NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (handleFor bct.Int32))
                )
            )

        let state = IlMachineState.writeManagedByref state bareLocallocPtr taggedHandle

        let reinterpretedPtr =
            bareLocallocPtr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs (concreteTypeFor bct.IntPtr))

        IlMachineState.readManagedByref bct state reinterpretedPtr
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual taggedHandle

    [<Test>]
    let ``readManagedByrefBytesAs with wrapped IntPtr template preserves tagged NativeIntSource`` () : unit =
        // Regression for `Unsafe.ReadUnaligned<IntPtr>` (Intrinsics.fs) and any
        // other byte-view caller whose `tZero` template comes from
        // `cliTypeZeroOfHandle` for a primitive-like wrapper. The template is
        // the wrapped value-type form of IntPtr; the byte-view read must still
        // recognise that a stored bare `Numeric (NativeInt ...)` cell — written
        // by e.g. `Stind_I` from another path — is byte-equivalent to the
        // requested template, so the fast path returns the cell as-is with its
        // tagged `NativeIntSource` provenance intact. Without the `haveSameCliShape`
        // widening, the read falls through to `StackMemoryPool.readBytes`,
        // which cannot serialise tagged sources and rejects the otherwise-valid
        // read.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let bareLocallocPtr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let taggedHandle =
            CliType.Numeric (
                CliNumericType.NativeInt (
                    NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed (handleFor bct.Int32))
                )
            )

        let state = IlMachineState.writeManagedByref state bareLocallocPtr taggedHandle

        let wrappedIntPtrTemplate, state =
            IlMachineState.cliTypeZeroOfHandle state bct (handleFor bct.IntPtr)

        // Sanity-check the template is the wrapped form (the regression
        // assumes that `cliTypeZeroOfHandle` returns a primitive-like wrapper
        // for `IntPtr`; if that ever changes, this regression must be revisited
        // because the exposure shape will be different).
        match wrappedIntPtrTemplate with
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsSome -> ()
        | other -> failwith $"expected cliTypeZeroOfHandle for IntPtr to return a primitive-like wrapper; got %O{other}"

        IlMachineState.readManagedByrefBytesAs bct state bareLocallocPtr wrappedIntPtrTemplate
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual taggedHandle

    [<Test>]
    let ``readManagedByrefBytesAs takes byte-walk path for non-primitive-like struct template over bare primitive cell``
        ()
        : unit
        =
        // Pins the half of `haveSameCliShape` that *doesn't* widen: non-primitive-like
        // value-type templates (e.g. multi-field structs) must not fast-path against
        // bare-primitive cells of the same size, because the resulting CLI shape would
        // be silently misinterpreted. The byte-walk fallback is correct here: it reads
        // the four bytes of the Int32 cell and reconstructs a `FourBytes` value with
        // the bytes populated as fields. If the comparator widened too far (e.g. a
        // future change collapsing `ValueType` shape comparison to size-equality),
        // this test would fail because the fast path would return the bare Int32 cell
        // verbatim.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let types = reinterpretWriteTypes loggerFactory

        let state, thread =
            stateWithSingleInstructionFromState loggerFactory (IlOp.Nullary NullaryIlOp.Nop) types.State

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let bareInt32Cell = CliType.Numeric (CliNumericType.Int32 0x04030201)
        let state = IlMachineState.writeManagedByref state ptr bareInt32Cell

        let fourBytesHandle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes types.FourBytesConcrete.Identity
            |> Option.defaultWith (fun () -> failwith "FourBytes handle missing")

        let fourBytesTemplate, state =
            IlMachineState.cliTypeZeroOfHandle state bct fourBytesHandle

        // Sanity-check the template is non-primitive-like.
        match fourBytesTemplate with
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsNone -> ()
        | other -> failwith $"expected FourBytes to be a non-primitive-like value type; got %O{other}"

        let result = IlMachineState.readManagedByrefBytesAs bct state ptr fourBytesTemplate

        match result with
        | CliType.ValueType vt ->
            vt.PrimitiveLikeKind |> shouldEqual None

            let readField (fieldIndex : int) : byte =
                match CliValueType.DereferenceFieldById types.FourBytesFields.[fieldIndex] vt with
                | CliType.Numeric (CliNumericType.UInt8 b) -> b
                | other -> failwith $"FourBytes::B%d{fieldIndex} was not UInt8: %O{other}"

            [| readField 0 ; readField 1 ; readField 2 ; readField 3 |]
            |> shouldEqual (System.BitConverter.GetBytes 0x04030201)
        | other -> failwith $"expected FourBytes-shaped result from byte-walk path; got %O{other}"

    [<Test>]
    let ``writeManagedByrefBytesOrTypedCell refuses to install wrapped IntPtr over bare tagged-NativeInt heap field``
        ()
        : unit
        =
        // Pins the strict (`sameCliConstructor`) shape predicate used by
        // `tryWriteHeapValueFieldPrecise`. A widened `haveSameCliShape` here
        // would let `WithFieldSetById` silently install a wrapped-IntPtr
        // `newValue` into a heap field whose stored shape is bare
        // `Numeric (NativeInt ...)`, corrupting the field's CLI shape (and,
        // in the tagged-source case, losing provenance to boot).
        //
        // The corruption scenario is the natural composition of two
        // already-supported flows: an earlier `Stind_I` deposits a
        // non-byte-addressable `NativeIntSource` (e.g. `FieldHandlePtr` from
        // `RuntimeFieldHandle.Value`) into a boxed `IntPtr`'s `_value`
        // slot through a `HeapObjectField` byref, and a later
        // `Unsafe.WriteUnaligned<IntPtr>` arrives over the box's `HeapValue`
        // byref carrying a wrapped-IntPtr `newValue` produced by
        // `cliTypeZeroOfHandle` + `toCliTypeCoerced`. With the strict
        // comparator the field-precise fast path declines on shape mismatch
        // and we fall through to byte scatter, which refuses the
        // non-byte-addressable target loudly. With the widened comparator
        // we would silently overwrite both the shape and the tag.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, _ =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let boxedAddr, state = allocateBoxedIntPtr 0L state
        let fieldId = intPtrValueFieldId ()
        let taggedSource = NativeIntSource.FieldHandlePtr 0xBEEFL

        let fieldPtr =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (boxedAddr, fieldId), [])

        let state =
            IlMachineState.writeManagedByref state fieldPtr (CliType.Numeric (CliNumericType.NativeInt taggedSource))

        // The boxed IntPtr's `_value` field really is bare NativeInt with
        // the tagged source — this is the precondition for the shape-
        // corruption regression. If a future refactor canonicalises the
        // field shape to wrapped form on install, this assertion will fail
        // before the regression check itself, surfacing the unrelated
        // representation drift rather than papering over it.
        ManagedHeap.get boxedAddr state.ManagedHeap
        |> _.Contents
        |> CliValueType.DereferenceFieldById fieldId
        |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt taggedSource))

        // The wrapped-IntPtr template `Unsafe.WriteUnaligned<IntPtr>`
        // produces via `cliTypeZeroOfHandle` + `EvalStackValue.toCliTypeCoerced`.
        let wrappedIntPtrNewValue, state =
            IlMachineState.cliTypeZeroOfHandle state bct (handleFor bct.IntPtr)

        match wrappedIntPtrNewValue with
        | CliType.ValueType vt when vt.PrimitiveLikeKind.IsSome -> ()
        | other -> failwith $"expected cliTypeZeroOfHandle for IntPtr to return a primitive-like wrapper; got %O{other}"

        let heapPtr = ManagedPointerSource.Byref (ByrefRoot.HeapValue boxedAddr, [])

        Assert.Throws<System.Exception> (fun () ->
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state heapPtr wrappedIntPtrNewValue
            |> ignore
        )
        |> ignore

        // The field cell is genuinely unchanged: same constructor (bare
        // NativeInt, not wrapped ValueType) and same tagged source. A
        // regression to the widened comparator would have installed the
        // wrapped value instead.
        ManagedHeap.get boxedAddr state.ManagedHeap
        |> _.Contents
        |> CliValueType.DereferenceFieldById fieldId
        |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt taggedSource))

    [<Test>]
    let ``Stind_I8 preserves tagged int64 provenance for exact-width typed destinations`` () : unit =
        let observedSources = HashSet<Int64Source> ()
        let observedDestinations = HashSet<TaggedInt64Destination> ()

        let property (source : Int64Source, destination : TaggedInt64Destination) : unit =
            observedSources.Add source |> ignore
            observedDestinations.Add destination |> ignore

            let _, loggerFactory = LoggerFactory.makeTest ()

            let state, thread =
                stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I8)

            let initial = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

            let ptr, state =
                match destination with
                | TaggedInt64Destination.StackMemory ->
                    let ptr, state =
                        IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

                    ptr, IlMachineState.writeManagedByref state ptr initial
                | TaggedInt64Destination.Int64ArrayElement ->
                    let arrayAddr, state = allocateInt64Array 1 state

                    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

            let state =
                state
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 source) thread

            let state =
                match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I8 with
                | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
                | other -> failwith $"Expected Stind_I8 to step, got %O{other}"

            IlMachineState.readManagedByref bct state ptr
            |> shouldEqual (CliType.Numeric (CliNumericType.Int64 source))

        Check.One (rawDataPropertyConfig.WithMaxTest 500, Prop.forAll (Arb.fromGen genTaggedInt64StindCase) property)

        // See the NativeInt property above: source count, rather than source
        // list equality, is the stable coverage assertion here.
        observedSources.Count |> shouldEqual ((taggedInt64Sources ()).Length)
        observedDestinations.Count |> shouldEqual 2

    [<Test>]
    let ``Exact-width provenance stind installs payload shape over same-width primitive slots`` () : unit =
        // Non-byte-renderable provenance cannot be faithfully scattered as
        // bytes. When the opcode width exactly matches the destination slot,
        // the typed store therefore records the payload's primitive shape
        // even if the previous same-width primitive template differed.
        let nativeIntSource = NativeIntSource.FieldHandlePtr 9876L
        let _, nativeIntLoggerFactory = LoggerFactory.makeTest ()

        let nativeIntState, nativeIntThread =
            stateWithSingleInstruction nativeIntLoggerFactory (IlOp.Nullary NullaryIlOp.Stind_I)

        let int64ArrayAddr, nativeIntState = allocateInt64Array 1 nativeIntState

        let int64Ptr =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (int64ArrayAddr, 0), [])

        let nativeIntState =
            nativeIntState
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer int64Ptr) nativeIntThread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt nativeIntSource) nativeIntThread

        let nativeIntState =
            match NullaryIlOp.execute nativeIntLoggerFactory bct nativeIntState nativeIntThread NullaryIlOp.Stind_I with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected Stind_I to step, got %O{other}"

        IlMachineState.getArrayValue int64ArrayAddr 0 nativeIntState
        |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt nativeIntSource))

        let int64Source =
            Int64Source.widenedNativeInt (NativeIntSource.FieldHandlePtr 6789L) true

        let _, int64LoggerFactory = LoggerFactory.makeTest ()

        let int64State, int64Thread =
            stateWithSingleInstruction int64LoggerFactory (IlOp.Nullary NullaryIlOp.Stind_I8)

        let nativeIntArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.IntPtr)

        let nativeIntArrayAddr, int64State =
            IlMachineState.allocateArray
                nativeIntArrayHandle
                (fun () -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                1
                int64State

        let nativeIntPtr =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (nativeIntArrayAddr, 0), [])

        let int64State =
            int64State
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer nativeIntPtr) int64Thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int64 int64Source) int64Thread

        let int64State =
            match NullaryIlOp.execute int64LoggerFactory bct int64State int64Thread NullaryIlOp.Stind_I8 with
            | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
            | other -> failwith $"Expected Stind_I8 to step, got %O{other}"

        IlMachineState.getArrayValue nativeIntArrayAddr 0 int64State
        |> shouldEqual (CliType.Numeric (CliNumericType.Int64 int64Source))

    [<Test>]
    let ``Stind_I through projected local-memory byte view reports provenance preservation failure`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_I)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let projectedPtr =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs (concreteTypeFor bct.Byte))

        let state =
            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer projectedPtr) thread
            |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.FieldHandlePtr 1234L)) thread

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_I |> ignore
            )

        ex.Message |> shouldContainText "primitive indirect store"
        ex.Message |> shouldContainText "cannot preserve new value's native int"

    [<Test>]
    let ``Stind_ref treats native-int-wrapped null managed pointer as managed null reference`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_ref)

        let state =
            state
            |> IlMachineState.pushToEvalStack'
                (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
                thread
            |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread

        match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_ref with
        | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) ->
            let activeFrame = state.ThreadState.[thread].ActiveMethodState
            let frame = IlMachineThreadState.getFrame thread activeFrame state

            frame.ExecutingMethod.Name |> shouldEqual ".ctor"
            frame.ExecutingMethod.DeclaringType.Name |> shouldEqual "NullReferenceException"

            match frame.ReturnState with
            | Some returnState ->
                returnState.DispatchAsExceptionOnReturn |> shouldEqual true
                returnState.WasConstructingObj |> Option.isSome |> shouldEqual true
            | None -> failwith "Expected NullReferenceException constructor frame to have a return state"
        | other -> failwith $"Expected Stind_ref wrapped null to raise a managed exception, got %O{other}"

    [<Test>]
    let ``Stind_ref through local-memory byte byref refuses to resize an existing cell`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_ref)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 8 state

        let state =
            IlMachineState.writeManagedByref state ptr (CliType.Numeric (CliNumericType.Int32 0x11223344))

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                let state =
                    state
                    |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
                    |> IlMachineState.pushToEvalStack' EvalStackValue.NullObjectRef thread

                NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_ref
                |> ignore
            )

        ex.Message |> shouldContainText "typed write"

        ex.Message
        |> shouldContainText "would replace an existing cell of size 4 with size 8"

    [<Test>]
    let ``Stind_ref keeps reference stores on typed byref path`` () : unit =
        // `stind.ref` is a reference-aware typed store, not a primitive
        // byte-scatter. If it accidentally routes through the primitive
        // indirect-store helper, the ObjectRef payload cannot be rendered as
        // bytes and this stops updating the array slot. The address may be
        // represented either directly as a managed pointer or as the
        // NativeInt-wrapped form produced by some stack transitions.
        let addressCases =
            [
                "managed pointer", fun ptr -> EvalStackValue.ManagedPointer ptr
                "native-int managed pointer", fun ptr -> EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr)
            ]

        for caseName, addressValue in addressCases do
            let _, loggerFactory = LoggerFactory.makeTest ()

            let state, thread =
                stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Stind_ref)

            let initialAddr, state = allocateReferenceObject state
            let replacementAddr, state = allocateReferenceObject state

            let objectArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Object)

            let arrayAddr, state =
                IlMachineState.allocateArray objectArrayHandle (fun () -> CliType.ObjectRef (Some initialAddr)) 1 state

            let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

            let state =
                state
                |> IlMachineState.pushToEvalStack' (addressValue ptr) thread
                |> IlMachineState.pushToEvalStack' (EvalStackValue.ObjectRef replacementAddr) thread

            let state =
                match NullaryIlOp.execute loggerFactory bct state thread NullaryIlOp.Stind_ref with
                | ExecutionResult.Stepped (state, WhatWeDid.Executed, _) -> state
                | other -> failwith $"Expected Stind_ref to step for %s{caseName}, got %O{other}"

            IlMachineState.getArrayValue arrayAddr 0 state
            |> shouldEqual (CliType.ObjectRef (Some replacementAddr))

    [<Test>]
    let ``Root reference-identical writes preserve state identity`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let op = IlOp.Nullary NullaryIlOp.Nop

        let localInitial = CliType.Numeric (CliNumericType.Int32 0x11223344)

        let localState, localThread =
            stateWithSingleInstructionAndLocals loggerFactory op (ImmutableArray.Create (handleFor bct.Int32))

        let localFrame = localState.ThreadState.[localThread].ActiveMethodState

        let localState =
            IlMachineState.setLocalVariable localThread localFrame 0us localInitial localState

        let localPtr =
            ManagedPointerSource.Byref (ByrefRoot.LocalVariable (localThread, localFrame, 0us), [])

        let localAfter = IlMachineState.writeManagedByref localState localPtr localInitial

        System.Object.ReferenceEquals (localAfter, localState) |> shouldEqual true

        let argumentInitial = CliType.Numeric (CliNumericType.Int32 0x55667788)
        let argumentState, argumentThread = stateWithSingleInstruction loggerFactory op
        let argumentFrame = argumentState.ThreadState.[argumentThread].ActiveMethodState

        let argumentState =
            IlMachineState.setArgument argumentThread argumentFrame 0us argumentInitial argumentState

        let argumentPtr =
            ManagedPointerSource.Byref (ByrefRoot.Argument (argumentThread, argumentFrame, 0us), [])

        let argumentAfter =
            IlMachineState.writeManagedByref argumentState argumentPtr argumentInitial

        System.Object.ReferenceEquals (argumentAfter, argumentState) |> shouldEqual true

        let staticInitial = CliType.Numeric (CliNumericType.Int32 0x10203040)
        let staticType = handleFor bct.Int32
        let staticFieldInfo = int32StaticField "MaxValue"
        let staticField = ComparableFieldDefinitionHandle.Make staticFieldInfo.Handle

        let staticState =
            state () |> IlMachineState.setStatic staticType staticField staticInitial

        let staticPtr =
            ManagedPointerSource.Byref (ByrefRoot.StaticField (staticType, staticField), [])

        let staticAfter =
            IlMachineState.writeManagedByref staticState staticPtr staticInitial

        System.Object.ReferenceEquals (staticAfter, staticState) |> shouldEqual true

        let heapAddr, heapState = allocateBoxedIntPtr 0x0102030405060708L (state ())
        let heapContents = boxedPayloadValueType heapAddr heapState
        let heapPtr = ManagedPointerSource.Byref (ByrefRoot.HeapValue heapAddr, [])

        let heapAfter =
            IlMachineState.writeManagedByref heapState heapPtr (CliType.ValueType heapContents)

        System.Object.ReferenceEquals (heapAfter, heapState) |> shouldEqual true

        let fieldAddr, fieldState = allocateBoxedIntPtr 0x0807060504030201L (state ())
        let fieldId = intPtrValueFieldId ()

        let fieldValue =
            ManagedHeap.get fieldAddr fieldState.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById fieldId

        let fieldPtr =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (fieldAddr, fieldId), [])

        let fieldAfter = IlMachineState.writeManagedByref fieldState fieldPtr fieldValue

        System.Object.ReferenceEquals (fieldAfter, fieldState) |> shouldEqual true

    [<Test>]
    let ``Bare boxed value byref byte view rejects object reference storage`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateObjectReferenceValue state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.HeapValue boxedAddr, [])

        assertReadWriteByteViewRejected
            state
            ptr
            [
                "refusing byte view"
                "boxed value type containing object references"
                "Boxed value layout:"
                "Obj: range=[0, 8), size=8"
                "byte-addressability: rejected: value type containing object references"
            ]

    [<Test>]
    let ``Bare boxed value byref byte view rejects runtime pointer storage`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateRuntimePointerValue state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.HeapValue boxedAddr, [])

        assertReadWriteByteViewRejected
            state
            ptr
            [
                "refusing byte view"
                "boxed value type containing runtime pointers"
                "Boxed value layout:"
                "Ptr: range=[0, 8), size=8"
                "byte-addressability: rejected: value type containing runtime pointers"
            ]

    [<Test>]
    let ``Array element byte view rejects object reference value-type storage`` () : unit =
        let state = state ()
        let valueType, state = objectReferenceValueType state
        let arrayAddr, state = allocateSingleValueTypeArray valueType state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        assertReadWriteByteViewRejected
            state
            ptr
            [
                "refusing byte view over value type containing object references in array "
                "Value layout:"
                "Obj: range=[0, 8), size=8"
                "byte-addressability: rejected: value type containing object references"
            ]

    [<Test>]
    let ``Array element byte view rejects runtime pointer value-type storage`` () : unit =
        let state = state ()
        let valueType = runtimePointerValueType state
        let arrayAddr, state = allocateSingleValueTypeArray valueType state
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        assertReadWriteByteViewRejected
            state
            ptr
            [
                "refusing byte view over value type containing runtime pointers in array "
                "Value layout:"
                "Ptr: range=[0, 8), size=8"
                "byte-addressability: rejected: value type containing runtime pointers"
            ]

    [<Test>]
    let ``RawData data projects array as byte byref before element 0`` () : unit =
        // CoreCLR's `Unsafe.As<RawData>(arr).Data` lands at the array's length-and-padding
        // header, `sizeof(nint)` bytes before the first element. PawPrint models that
        // "before-element-0" position by anchoring at `ArrayElement(arr, 0)` with a trailing
        // negative `ByteOffset` so the canonical `+sizeof(nint)` skip used by callers like
        // `CastCache.TableData` collapses cleanly to `&array[0]` via the existing
        // `ManagedPointerSource` offset arithmetic.
        let state = state ()
        let arrayAddr, state = allocateIntArray 1 state
        let byteView = concreteTypeFor bct.Byte

        let nativeIntSize =
            CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

        let ptr = projectRawDataDataPointer arrayAddr state

        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actualAddr, 0),
                                      [ ByrefProjection.ReinterpretAs view ; ByrefProjection.ByteOffset offset ]) ->
            actualAddr |> shouldEqual arrayAddr
            view |> shouldEqual byteView
            offset |> shouldEqual (-nativeIntSize)
        | other -> failwith $"Expected RawData::Data on array to project as byte byref before element 0, got %O{other}"

        // The canonical `+sizeof(nint)` skip should collapse the negative offset, leaving
        // a clean byte byref at `&array[0]`.
        let skipped =
            ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset nativeIntSize) ptr

        match skipped with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actualAddr, 0), [ ByrefProjection.ReinterpretAs view ]) ->
            actualAddr |> shouldEqual arrayAddr
            view |> shouldEqual byteView
        | other ->
            failwith
                $"Expected canonical +sizeof(nint) skip to collapse to a clean byte byref at &array[0], got %O{other}"

    [<Test>]
    let ``RawData data projection rejects multi-dimensional arrays`` () : unit =
        // CoreCLR places `2 * rank` int32 bounds entries between the length header and the
        // first element of an MD array (`MethodTableProjection.baseSize` models this as
        // `(3 + rank) * NATIVE_INT_SIZE`). The SZ-array projection's canonical
        // `+sizeof(nint)` skip would therefore not land on element 0 for MD arrays, so the
        // projection must refuse them rather than silently produce a byref to the bounds
        // region. We fail loudly with a TODO that names the rank so a future caller knows
        // exactly what needs modelling.
        let state = state ()

        let arrayType = ConcreteTypeHandle.Array (handleFor bct.Int32, 2)

        let arrayAddr, state =
            IlMachineState.allocateMultiDimArray
                arrayType
                (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                (ImmutableArray.Create<int> (2, 3))
                state

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                RuntimeFieldProjection.tryProjectFieldAddress bct (rawDataField "Data") arrayAddr state
                |> ignore
            )

        ex.Message
        |> shouldContainText "TODO: RawData::Data projection for multi-dimensional array (rank 2)"

    [<Test>]
    let ``RawData data projects reference-type heap object as a byte-view byref`` () : unit =
        // EventSource initialisation reaches `obj.GetRawData()` over `OverrideEventProvider`
        // (a class), expecting a byref into the instance data so subsequent `Unsafe.AddByteOffset`
        // and `Unsafe.As<byte, object>` arithmetic can reach a reference field. Reference-type
        // heap objects must therefore project the same byte-view shape as boxed value types;
        // the value-type-only restriction was historical and is reinstated only as
        // field-precise dispatch when the byte view is later resolved.
        let state = state ()
        let _, containerAddr, state = allocateReferenceObjectWithRefField state

        let ptr = projectRawDataDataPointer containerAddr state

        match ptr with
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue actualAddr, [ ByrefProjection.ReinterpretAs view ]) ->
            actualAddr |> shouldEqual containerAddr
            view |> shouldEqual (concreteTypeFor bct.Byte)
        | other -> failwith $"Expected RawData::Data byte-view byref over reference-type heap object, got %O{other}"

    [<Test>]
    let ``Heap object byte view reads ObjectRef field via field-precise dispatch`` () : unit =
        // The byte-view shape `[ReinterpretAs byte; ByteOffset 0]` over a heap object whose
        // storage carries a single 8-byte ObjectRef field at offset 0 must, when read back as
        // ObjectRef, recover the field's identity. Falling through to byte-walk would either
        // reject or zero-pad the reference, both of which destroy identity.
        let state = state ()
        let storedAddr, containerAddr, state = allocateReferenceObjectWithRefField state

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.ObjectRef None)
        |> shouldEqual (CliType.ObjectRef (Some storedAddr))

    [<Test>]
    let ``Heap object byte view writes ObjectRef field via field-precise dispatch`` () : unit =
        let state = state ()
        let _, containerAddr, state = allocateReferenceObjectWithRefField state
        let replacementAddr, state = allocateReferenceObject state

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr (CliType.ObjectRef (Some replacementAddr))

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.ObjectRef None)
        |> shouldEqual (CliType.ObjectRef (Some replacementAddr))

        // Field-precise writes must update the typed field cell, not just produce a byte image
        // that round-trips on read. A direct field dereference is the strongest evidence that
        // identity was preserved through the byte-view shape.
        ManagedHeap.get containerAddr state.ManagedHeap
        |> AllocatedNonArrayObject.DereferenceField "Ref"
        |> shouldEqual (CliType.ObjectRef (Some replacementAddr))

    [<Test>]
    let ``Heap object byte view writing null preserves field shape`` () : unit =
        let state = state ()
        let _, containerAddr, state = allocateReferenceObjectWithRefField state

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell bct state ptr (CliType.ObjectRef None)

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.ObjectRef None)
        |> shouldEqual (CliType.ObjectRef None)

        ManagedHeap.get containerAddr state.ManagedHeap
        |> AllocatedNonArrayObject.DereferenceField "Ref"
        |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``Heap object byte view still rejects byte read at ObjectRef field offset`` () : unit =
        // Field-precise dispatch is shape-matched: it kicks in only when the byte-view
        // template aligns to a field of the same shape and size. A `UInt8` read at offset 0
        // of an 8-byte ObjectRef field cannot be answered by inspecting the field — the
        // single-byte value would have no defined byte image — and must continue to reject.
        let state = state ()
        let _, containerAddr, state = allocateReferenceObjectWithRefField state

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.UInt8 0uy))
                |> ignore
            )

        ex.Message |> shouldContainText "refusing byte view"

    [<Test>]
    let ``Heap object byte view rejects ObjectRef read at field-misaligned offset`` () : unit =
        // Reading an ObjectRef shape but at byte offset 4 (mid-field) must not silently
        // produce the shifted-by-4 alias of the field's value: there is no such cell, and the
        // closest field-precise match is at offset 0 with size 8. Reject rather than guess.
        let state = state ()
        let _, containerAddr, state = allocateReferenceObjectWithRefField state

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 4)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.ObjectRef None)
                |> ignore
            )

        ex.Message |> shouldContainText "refusing byte view"

    [<Test>]
    let ``Heap object byte view reads ObjectRef via Unsafe.As<byte, object> chain`` () : unit =
        // EventSource initialisation reaches a reference field through:
        //   1. `obj.GetRawData()`, projecting `[ReinterpretAs byte]`.
        //   2. `Unsafe.AddByteOffset(ref byte0, N)`, appending `ByteOffset N`.
        //   3. `Unsafe.As<byte, object>(ref byteN)`, appending `ReinterpretAs object`.
        // After `appendProjection` collapses the chained reinterpret, the projection list is
        // `[ReinterpretAs object]` (or `[ReinterpretAs object; ByteOffset N]` for non-zero N).
        // `readManagedByref` must drive `readManagedByrefBytesAs` with the concrete type's
        // zero-value template (an `ObjectRef None` for `Object`), which then dispatches through
        // field-precise read and recovers the original reference. Without that, the read falls
        // through to a generic byte view and fails with "struct/object byte views are not modelled".
        let state = state ()
        let storedAddr, containerAddr, state = allocateReferenceObjectWithRefField state
        let objectType = concreteTypeFor bct.Object

        let ptr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs objectType)

        IlMachineState.readManagedByref bct state ptr
        |> shouldEqual (CliType.ObjectRef (Some storedAddr))

    [<Test>]
    let ``Heap object byte view preserves overlap semantics for byte-addressable fields`` () : unit =
        // Field-precise dispatch is gated on non-byte-addressability so that explicit-layout
        // overlap semantics remain authoritative for primitive cells. Setup: a heap object with
        // an Int32 field A at offset 0 and an Int16 field B at offset 2. Initial bytes are
        // [11 11 11 11], with A = 0x11111111 and B = 0x1111. A byte-view write of Int16 0xCAFE
        // at offset 2 must canonicalise through `WithBytesAtIfChanged` + `OfBytesLike`, so that
        // a subsequent byte-view read of Int32 at offset 0 reflects the new bytes for B's slice
        // (returning 0xCAFE1111). Were field-precise to fire for byte-addressable fields, the
        // write would only update B's cell and the Int32 read would observe A's stale 0x11111111
        // instead of the canonical [11 11 FE CA] byte image. This test only catches regressions
        // when both gates are simultaneously absent, but it pins the round-trip semantic.
        let state = state ()
        let int32Handle = handleFor bct.Int32
        let int16Handle = handleFor bct.Int16
        let objectHandle = handleFor bct.Object

        let containerFields =
            [
                {
                    Id = FieldId.named "A"
                    Name = "A"
                    Contents = CliType.Numeric (CliNumericType.Int32 0x11111111)
                    Offset = Some 0
                    Type = int32Handle
                    MarshallingDescriptor = None
                }
                {
                    Id = FieldId.named "B"
                    Name = "B"
                    Contents = CliType.Numeric (CliNumericType.Int16 0x1111s)
                    Offset = Some 2
                    Type = int16Handle
                    MarshallingDescriptor = None
                }
            ]
            |> CliValueType.OfFields
                bct
                state.ConcreteTypes
                objectHandle
                (Layout.Custom (size = 4, packingSize = 0))
                CharSet.Ansi

        let containerAddr, state =
            IlMachineState.allocateManagedObject objectHandle containerFields state

        let writePtr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 2)

        let state =
            IlMachineState.writeManagedByrefBytesOrTypedCell
                bct
                state
                writePtr
                (CliType.Numeric (CliNumericType.Int16 0xCAFEs))

        let readPtr =
            projectRawDataDataPointer containerAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 0)

        IlMachineState.readManagedByrefBytesAs bct state readPtr (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0xCAFE1111))

        // The Int16 byte-view read of B must also recover 0xCAFE through the byte-walk path:
        // because B is byte-addressable, field-precise dispatch defers to `BytesAt`, which
        // observes the canonical overlay rather than B's tracked cell.
        IlMachineState.readManagedByrefBytesAs bct state writePtr (CliType.Numeric (CliNumericType.Int16 0s))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int16 0xCAFEs))

    [<Test>]
    let ``RawData boxed value byte view bounds checks reads and writes`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state

        let ptrAtOffset =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 7)

        let readEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    ptrAtOffset
                    (CliType.Numeric (CliNumericType.UInt16 0us))
                |> ignore
            )

        readEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let negativePtr =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset -1)

        let negativeReadEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs
                    bct
                    state
                    negativePtr
                    (CliType.Numeric (CliNumericType.UInt16 0us))
                |> ignore
            )

        negativeReadEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let negativeWriteEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    negativePtr
                    (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))
                |> ignore
            )

        negativeWriteEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let writeEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytesOrTypedCell
                    bct
                    state
                    ptrAtOffset
                    (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))
                |> ignore
            )

        writeEx.Message |> shouldContainText "outside 8-byte boxed payload"

    [<Test>]
    let ``Metadata-light writeManagedByref accepts trailing ReinterpretAs byte view`` () : unit =
        // Regression: `writeManagedByref` is the BCT-less entry point used by
        // primitive/external boundaries that do not currently carry type
        // metadata. Historically it accepted simple trailing byte-view shapes
        // (`[ReinterpretAs T]` and `[..., ReinterpretAs T; ByteOffset n]`) over
        // byte-addressable roots, routing through the byte-scatter path of
        // `writeManagedByrefBytesOrTypedCell`. The forward-walk peel rewrite
        // initially required BCT, which broke this metadata-light contract;
        // this test pins the restored behaviour for both the bare reinterpret
        // shape and the reinterpret-plus-byte-offset shape.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let state, thread =
            stateWithSingleInstruction loggerFactory (IlOp.Nullary NullaryIlOp.Nop)

        let ptr, state =
            IlMachineState.allocateStackMemory thread MemoryBlockInitialization.ZeroInitialized 4 state

        let byteReinterpret = concreteTypeFor bct.Byte

        let bareReinterpretPtr =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ReinterpretAs byteReinterpret)

        let state =
            IlMachineState.writeManagedByref state bareReinterpretPtr (CliType.Numeric (CliNumericType.UInt8 0xAAuy))

        IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.UInt8 0uy))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0xAAuy))

        let offsetReinterpretPtr =
            bareReinterpretPtr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 2)

        let state =
            IlMachineState.writeManagedByref state offsetReinterpretPtr (CliType.Numeric (CliNumericType.UInt8 0xBBuy))

        let readAt (offset : int) : CliType =
            let readPtr =
                bareReinterpretPtr
                |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset offset)

            IlMachineState.readManagedByrefBytesAs bct state readPtr (CliType.Numeric (CliNumericType.UInt8 0uy))

        readAt 0 |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0xAAuy))
        readAt 2 |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0xBBuy))
