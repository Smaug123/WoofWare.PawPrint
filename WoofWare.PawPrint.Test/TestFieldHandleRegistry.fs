namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFieldHandleRegistry =

    [<Test>]
    let ``Ldtoken field handle stores RuntimeFieldInfoStub object`` () : unit =
        let source =
            """
public static class HasField
{
    public static int Data = 1;
    public static int Other = 2;
}
"""

        let image =
            Roslyn.compileAssembly
                "FieldHandleTestAssembly"
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

        let state =
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

        let runtimeFieldInfoStubAddress (fieldHandle : CliType) : ManagedHeapAddress =
            match fieldHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_ptr" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"Expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
            | other -> failwith $"Expected RuntimeFieldHandle value type, got %O{other}"

        let fieldHandle, state =
            IlMachineState.getOrAllocateField loggerFactory baseClassTypes assembly.Name field.Handle state

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle

        let allocated = ManagedHeap.get runtimeFieldInfoStubAddr state.ManagedHeap

        let runtimeFieldInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.RuntimeFieldInfoStub

        allocated.ConcreteType |> shouldEqual runtimeFieldInfoStubType

        let fieldHandleId =
            match CliValueType.DereferenceField "m_fieldHandle" allocated.Contents with
            | CliType.ValueType runtimeFieldHandleInternal ->
                match CliValueType.DereferenceField "m_handle" runtimeFieldHandleInternal with
                | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> id
                | other ->
                    failwith
                        $"Expected RuntimeFieldHandleInternal.m_handle to be a field-registry handle, got %O{other}"
            | other ->
                failwith
                    $"Expected RuntimeFieldInfoStub.m_fieldHandle to be a RuntimeFieldHandleInternal, got %O{other}"

        let resolved =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        let resolvedId =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddr}"
            )

        resolvedId |> shouldEqual fieldHandleId

        FieldHandleRegistry.resolveFieldIdFromAddress
            (ManagedHeapAddress state.ManagedHeap.FirstAvailableAddress)
            state.FieldHandles
        |> shouldEqual None

        let fieldHandleAgain, state =
            IlMachineState.getOrAllocateField loggerFactory baseClassTypes assembly.Name field.Handle state

        let runtimeFieldInfoStubAddrAgain = runtimeFieldInfoStubAddress fieldHandleAgain

        runtimeFieldInfoStubAddrAgain |> shouldEqual runtimeFieldInfoStubAddr

        let resolvedIdAgain =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddrAgain state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddrAgain}"
            )

        resolvedIdAgain |> shouldEqual fieldHandleId

        let otherFieldHandle, state =
            IlMachineState.getOrAllocateField loggerFactory baseClassTypes assembly.Name otherField.Handle state

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

        otherResolved.GetFieldDefinitionHandle().Get |> shouldEqual otherField.Handle

        resolved.GetAssemblyFullName () |> shouldEqual assembly.Name.FullName
        resolved.GetFieldDefinitionHandle().Get |> shouldEqual field.Handle

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
