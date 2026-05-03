namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
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

    let private allocateIntArray (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Int32)

        IlMachineState.allocateArray intArrayHandle (fun () -> CliType.Numeric (CliNumericType.Int32 0)) length state

    let private allocateBoxedIntPtr (bits : int64) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let intPtrHandle = handleFor bct.IntPtr

        let valueField =
            bct.IntPtr.Fields
            |> List.filter (fun field -> field.Name = "_value" && not field.IsStatic)
            |> List.exactlyOne

        let valueType =
            [
                {
                    Id = FieldId.metadata intPtrHandle valueField.Handle valueField.Name
                    Name = valueField.Name
                    Contents = CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits))
                    Offset = valueField.Offset
                    Type = intPtrHandle
                }
            ]
            |> CliValueType.OfFields bct state.ConcreteTypes intPtrHandle Layout.Default

        IlMachineState.allocateManagedObject intPtrHandle valueType state

    let private allocateReferenceObject (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let objectHandle = handleFor bct.Object

        let objectValue =
            CliValueType.OfFields bct state.ConcreteTypes objectHandle Layout.Default []

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
                }
            ]
            |> CliValueType.OfFields bct state.ConcreteTypes declared (Layout.Custom (size = 8, packingSize = 0))

        valueType, state

    let private runtimePointerValueType (state : IlMachineState) : CliValueType =
        let declared = handleFor bct.TypedReference
        let intPtrHandle = handleFor bct.IntPtr
        let intHandle = handleFor bct.Int32

        [
            {
                Id = FieldId.named "Ptr"
                Name = "Ptr"
                Contents = CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr intHandle)
                Offset = Some 0
                Type = intPtrHandle
            }
        ]
        |> CliValueType.OfFields bct state.ConcreteTypes declared (Layout.Custom (size = 8, packingSize = 0))

    let private allocateObjectReferenceValue (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let valueType, state = objectReferenceValueType state

        IlMachineState.allocateManagedObject valueType.Declared valueType state

    let private allocateRuntimePointerValue (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let valueType = runtimePointerValueType state

        IlMachineState.allocateManagedObject valueType.Declared valueType state

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
                IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.UInt8 0uy))
                |> ignore
            )

        for fragment in expectedFragments do
            readEx.Message |> shouldContainText fragment

        let writeEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.UInt8 0xAAuy))
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

    type private SignedZeroWriteCase =
        {
            InitialNegative : bool
            WrittenNegative : bool
        }

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
    let private categoryMask : int32 = 0x000C0000
    let private categoryInterface : int32 = 0x000C0000
    let private categoryArray : int32 = 0x00080000
    let private componentSizeMask : int32 = 0x0000FFFF

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
                target
                (state ())
        with
        | None -> failwith $"Expected MethodTableAuxiliaryData::{fieldName} to project"
        | Some (result, _) -> result

    let private methodWithSingleInstruction
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (op : IlOp)
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
            }

        let method =
            objectToString
            |> MethodInfo.mapTypeGenerics (fun _ -> failwith "System.Object::ToString is not type-generic")
            |> MethodInfo.mapMethodGenerics (fun _ _ -> failwith "System.Object::ToString is not method-generic")
            |> MethodInfo.setMethodVars (Some instructions) signature

        state, method

    let private stateWithSingleInstruction (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) (op : IlOp) =
        let state, method = state () |> methodWithSingleInstruction loggerFactory op

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
                (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr intArrayHandle))
                thread

        let state, whatWeDid =
            UnaryMetadataIlOp.execute loggerFactory bct UnaryMetadataTokenIlOp.Ldfld token state thread

        whatWeDid |> shouldEqual WhatWeDid.Executed

        IlMachineState.peekEvalStack thread state
        |> shouldEqual (Some (EvalStackValue.Int32 (hasComponentSizeFlag ||| categoryArray ||| 4)))

        state.ThreadState.[thread].MethodState.IlOpIndex
        |> shouldEqual (IlOp.NumberOfBytes op)

    [<Test>]
    let ``ElementType preserves MethodTable pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "ElementType" (ConcreteTypeHandle.OneDimArrayZero intHandle)
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr intHandle))

    [<Test>]
    let ``AuxiliaryData preserves MethodTable auxiliary-data pointer provenance`` () : unit =
        let intHandle = handleFor bct.Int32

        project "AuxiliaryData" intHandle
        |> shouldEqual (CliType.RuntimePointer (CliRuntimePointer.MethodTableAuxiliaryDataPtr intHandle))

    [<Test>]
    let ``AuxiliaryData flags start with fast-compare cache bits unset`` () : unit =
        projectAuxiliaryData "Flags" (handleFor bct.Int32)
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

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
                (EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr intHandle))
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
            IlMachineState.writeManagedByrefBytes state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))

        let updated =
            ManagedHeap.get boxedAddr state.ManagedHeap
            |> _.Contents
            |> CliValueType.ToBytes

        updated |> shouldEqual expectedBytes

        IlMachineState.readManagedByrefBytesAs state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))

    [<Test>]
    let ``RawData boxed value byte view reads original boxed storage`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state

        let ptrAtOffset =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 2)

        IlMachineState.readManagedByrefBytesAs state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
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
                IlMachineState.writeManagedByrefBytes
                    state
                    ptrAtOffset
                    (CliType.Numeric (CliNumericType.UInt16 sample.Payload))

            boxedPayloadBytes boxedAddr state |> shouldEqual expectedBytes

            IlMachineState.readManagedByrefBytesAs state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
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
                IlMachineState.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.UInt16 payload))

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

        ex.Message |> shouldContainText "byte-unaddressable storage (object reference)"
        ex.Message |> shouldContainText "write through `ReinterpretAs`"
        ex.Message |> shouldContainText "CLI value byte layout:"

        ex.Message
        |> shouldContainText "byte-addressability: rejected: object reference"

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

        ex.Message
        |> shouldContainText "byte-unaddressable storage (value type containing runtime pointers)"

        ex.Message |> shouldContainText "write through `ReinterpretAs`"
        ex.Message |> shouldContainText "value type byte layout:"
        ex.Message |> shouldContainText "declared type:"
        ex.Message |> shouldContainText "Ptr: range=[0, 8), size=8"

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

        IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.UInt16 0us))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt16 0x0708us))

        let state =
            IlMachineState.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.Int32 replacement))

        boxedPayloadBytes boxedAddr state |> shouldEqual expectedBytes

        IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.Int32 0))
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
                IlMachineState.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.UInt16 payload))

            let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]
            let payloadAfter = arrayElementValueType arrayAddr 0 state

            System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true
            System.Object.ReferenceEquals (payloadAfter, payloadBefore) |> shouldEqual true

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genByteIdenticalUInt16WriteCase) property)

    [<Test>]
    let ``Array primitive NaN byte-identical write preserves array identity`` () : unit =
        let state = state ()
        let nan = CliType.Numeric (CliNumericType.Float64 System.Double.NaN)
        let doubleArrayHandle = ConcreteTypeHandle.OneDimArrayZero (handleFor bct.Double)

        let arrayAddr, state =
            IlMachineState.allocateArray doubleArrayHandle (fun () -> nan) 1 state

        let arrayBefore = state.ManagedHeap.Arrays.[arrayAddr]
        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let state = IlMachineState.writeManagedByrefBytes state ptr nan
        let arrayAfter = state.ManagedHeap.Arrays.[arrayAddr]

        System.Object.ReferenceEquals (arrayAfter, arrayBefore) |> shouldEqual true

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
            CliType.Numeric (CliNumericType.Int64 (System.BitConverter.ToInt64 (writtenBytes, 0)))

        let ptr = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])
        let state = IlMachineState.writeManagedByrefBytes state ptr written
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
            let state = IlMachineState.writeManagedByrefBytes state ptr written

            let actual =
                IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.Float64 0.0))

            CliType.ToBytes actual |> shouldEqual (CliType.ToBytes written)

        Check.One (rawDataPropertyConfig, Prop.forAll (Arb.fromGen genSignedZeroWriteCase) property)

        for initialNegative in [ false ; true ] do
            for writtenNegative in [ false ; true ] do
                observed.Contains ((initialNegative, writtenNegative)) |> shouldEqual true

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
                "byte-view over value type containing object references in array "
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
                "byte-view over value type containing runtime pointers in array "
                "Value layout:"
                "Ptr: range=[0, 8), size=8"
                "byte-addressability: rejected: value type containing runtime pointers"
            ]

    [<Test>]
    let ``RawData data projection rejects array addresses`` () : unit =
        let state = state ()
        let arrayAddr, state = allocateIntArray 1 state

        let ex =
            Assert.Throws<System.Exception> (fun () -> projectRawDataDataPointer arrayAddr state |> ignore)

        ex.Message |> shouldContainText "got array"

    [<Test>]
    let ``RawData data projection rejects reference type objects`` () : unit =
        let state = state ()
        let objectAddr, state = allocateReferenceObject state

        let ex =
            Assert.Throws<System.Exception> (fun () -> projectRawDataDataPointer objectAddr state |> ignore)

        ex.Message |> shouldContainText "expected boxed value type"
        Assert.That (ex.Message, Does.Not.Contain "got array")

    [<Test>]
    let ``RawData boxed value byte view bounds checks reads and writes`` () : unit =
        let state = state ()
        let boxedAddr, state = allocateBoxedIntPtr 0x0102030405060708L state

        let ptrAtOffset =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 7)

        let readEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs state ptrAtOffset (CliType.Numeric (CliNumericType.UInt16 0us))
                |> ignore
            )

        readEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let negativePtr =
            projectRawDataDataPointer boxedAddr state
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset -1)

        let negativeReadEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs state negativePtr (CliType.Numeric (CliNumericType.UInt16 0us))
                |> ignore
            )

        negativeReadEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let negativeWriteEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytes
                    state
                    negativePtr
                    (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))
                |> ignore
            )

        negativeWriteEx.Message |> shouldContainText "outside 8-byte boxed payload"

        let writeEx =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.writeManagedByrefBytes
                    state
                    ptrAtOffset
                    (CliType.Numeric (CliNumericType.UInt16 0xBEEFus))
                |> ignore
            )

        writeEx.Message |> shouldContainText "outside 8-byte boxed payload"
