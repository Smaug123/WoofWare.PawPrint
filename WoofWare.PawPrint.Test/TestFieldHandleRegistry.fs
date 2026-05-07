namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFieldHandleRegistry =

    let private fieldHandleSource =
        """
public static class HasField
{
    public static int Data = 1;
    public static int Other = 2;
}
"""

    type private FieldHandleFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            Field : FieldInfo<GenericParamFromMetadata, TypeDefn>
            OtherField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            State : IlMachineState
        }

    let private makeFieldHandleFixture () : FieldHandleFixture =
        let image =
            Roslyn.compileAssembly
                "FieldHandleTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ fieldHandleSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let field =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "HasField" && field.Name = "Data")

        let otherField =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "HasField" && field.Name = "Other")

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib.Name corelib

        let state : IlMachineState =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.RuntimeFieldHandle
                 baseClassTypes.RuntimeFieldHandleInternal
                 baseClassTypes.RuntimeFieldInfoStub
             ])
            ||> List.fold (fun state ty ->
                let typeDefn =
                    DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies ty

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
            )

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Assembly = assembly
            Field = field
            OtherField = otherField
            State = state
        }

    let private getOrAllocateField
        (fixture : FieldHandleFixture)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        IlMachineState.getOrAllocateField
            fixture.LoggerFactory
            fixture.BaseClassTypes
            fixture.Assembly.Name
            field.Handle
            state

    let private runtimeFieldInfoStubAddress (fieldHandle : CliType) : ManagedHeapAddress =
        match fieldHandle with
        | CliType.ValueType vt ->
            match CliValueType.DereferenceField "m_ptr" vt with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwith $"Expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
        | other -> failwith $"Expected RuntimeFieldHandle value type, got %O{other}"

    let private fieldHandleIdInRuntimeFieldInfoStub (allocated : AllocatedNonArrayObject) : int64 =
        match CliValueType.DereferenceField "m_fieldHandle" allocated.Contents with
        | CliType.ValueType runtimeFieldHandleInternal ->
            match CliValueType.DereferenceField "m_handle" runtimeFieldHandleInternal with
            | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> id
            | other ->
                failwith $"Expected RuntimeFieldHandleInternal.m_handle to be a field-registry handle, got %O{other}"
        | other ->
            failwith $"Expected RuntimeFieldInfoStub.m_fieldHandle to be a RuntimeFieldHandleInternal, got %O{other}"

    let private fieldHandleIdAtAddress (address : ManagedHeapAddress) (state : IlMachineState) : int64 =
        ManagedHeap.get address state.ManagedHeap |> fieldHandleIdInRuntimeFieldInfoStub

    let private allocatePlainObject
        (fixture : FieldHandleFixture)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let objectType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let contents =
            ([] : CliField list)
            |> CliValueType.OfFields fixture.BaseClassTypes state.ConcreteTypes objectType Layout.Default

        IlMachineState.allocateManagedObject objectType contents state

    [<Test>]
    let ``Field handle allocation stores RuntimeFieldInfoStub object`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let allocated = ManagedHeap.get runtimeFieldInfoStubAddr state.ManagedHeap

        let runtimeFieldInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.RuntimeFieldInfoStub

        allocated.ConcreteType |> shouldEqual runtimeFieldInfoStubType

        let fieldHandleId = fieldHandleIdInRuntimeFieldInfoStub allocated

        let resolved =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        resolved.GetAssemblyFullName () |> shouldEqual fixture.Assembly.Name.FullName
        resolved.GetFieldDefinitionHandle().Get |> shouldEqual fixture.Field.Handle

    [<Test>]
    let ``RuntimeFieldInfoStub address resolves to field handle id`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let resolvedId =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddr}"
            )

        resolvedId |> shouldEqual fieldHandleId

        let resolvedFromAddress =
            FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddr}"
            )

        let resolvedFromId =
            FieldHandleRegistry.resolveFieldFromId resolvedId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{resolvedId}")

        resolvedFromId.GetAssemblyFullName ()
        |> shouldEqual (resolvedFromAddress.GetAssemblyFullName ())

        resolvedFromId.GetFieldDefinitionHandle().Get
        |> shouldEqual (resolvedFromAddress.GetFieldDefinitionHandle().Get)

    [<Test>]
    let ``Unknown or non-field-stub addresses do not resolve to field handle ids`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let _, state = getOrAllocateField fixture fixture.Field fixture.State

        FieldHandleRegistry.resolveFieldIdFromAddress
            (ManagedHeapAddress state.ManagedHeap.FirstAvailableAddress)
            state.FieldHandles
        |> shouldEqual None

        let objectAddress, state = allocatePlainObject fixture state

        FieldHandleRegistry.resolveFieldIdFromAddress objectAddress state.FieldHandles
        |> shouldEqual None

        FieldHandleRegistry.resolveFieldIdFromAddress
            (ManagedHeapAddress state.ManagedHeap.FirstAvailableAddress)
            state.FieldHandles
        |> shouldEqual None

    [<Test>]
    let ``Reallocating a field preserves its field-stub address and id`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let fieldHandleAgain, state = getOrAllocateField fixture fixture.Field state

        let runtimeFieldInfoStubAddrAgain = runtimeFieldInfoStubAddress fieldHandleAgain

        runtimeFieldInfoStubAddrAgain |> shouldEqual runtimeFieldInfoStubAddr

        let resolvedIdAgain =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddrAgain state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddrAgain}"
            )

        resolvedIdAgain |> shouldEqual fieldHandleId

    [<Test>]
    let ``Different fields resolve to different field-stub addresses and ids`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let otherFieldHandle, state = getOrAllocateField fixture fixture.OtherField state

        let otherRuntimeFieldInfoStubAddr = runtimeFieldInfoStubAddress otherFieldHandle

        otherRuntimeFieldInfoStubAddr |> shouldNotEqual runtimeFieldInfoStubAddr

        let otherFieldHandleId =
            FieldHandleRegistry.resolveFieldIdFromAddress otherRuntimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{otherRuntimeFieldInfoStubAddr}"
            )

        otherFieldHandleId |> shouldNotEqual fieldHandleId

        let otherResolved =
            FieldHandleRegistry.resolveFieldFromId otherFieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{otherFieldHandleId}")

        otherResolved.GetFieldDefinitionHandle().Get
        |> shouldEqual fixture.OtherField.Handle

        let originalResolved =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        originalResolved.GetFieldDefinitionHandle().Get
        |> shouldEqual fixture.Field.Handle

    [<Test>]
    let ``RVA field data can be read through managed byte pointer`` () : unit =
        let source =
            """
using System;

public static class HasRvaData
{
    public static int Length()
    {
        ReadOnlySpan<byte> bytes = new byte[] { 0x11, 0x22, 0x33, 0x44, 0x55 };
        return bytes.Length;
    }
}
"""

        let image =
            Roslyn.compileAssembly
                "RvaFieldTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let rvaField =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.RelativeVirtualAddress.IsSome)

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib.Name corelib

        let state, peByteRange =
            IlMachineState.peByteRangeForFieldRva
                loggerFactory
                baseClassTypes
                assembly
                rvaField
                ImmutableArray.Empty
                state

        let peByteRange =
            peByteRange
            |> Option.defaultWith (fun () ->
                failwith "Expected compiler-generated field to have a field-RVA PE byte range"
            )

        peByteRange.Size |> shouldEqual 5

        let state, ptr =
            IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

        let byteTemplate = CliType.Numeric (CliNumericType.UInt8 0uy)

        ManagedPointerSource.tryStableAddressBits ptr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress))

        IlMachineState.readManagedByrefBytesAs state ptr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0x11uy))

        let offsetPtr =
            ptr |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 4)

        ManagedPointerSource.tryStableAddressBits offsetPtr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress + 4L))

        offsetPtr
        |> fun ptr -> IlMachineState.readManagedByrefBytesAs state ptr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0x55uy))

        let outOfBoundsPtr =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset peByteRange.Size)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs state outOfBoundsPtr byteTemplate
                |> ignore
            )

        ex.Message.Contains "outside byte range size" |> shouldEqual true
