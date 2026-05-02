namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

[<TestFixture>]
module TestManifestResources =
    let private embeddedResource (resourceName : string) (resourceBytes : byte array) : ResourceDescription =
        ResourceDescription (resourceName, Func<Stream> (fun () -> new MemoryStream (resourceBytes) :> Stream), true)

    let private linkedResource
        (resourceName : string)
        (fileName : string)
        (resourceBytes : byte array)
        : ResourceDescription
        =
        ResourceDescription (
            resourceName,
            fileName,
            Func<Stream> (fun () -> new MemoryStream (resourceBytes) :> Stream),
            true
        )

    let private compileResourceAssemblyImage (resources : ResourceDescription list) : byte[] =
        let source =
            """
public static class Entry
{
    public static int Value => 1;
}
"""

        Roslyn.compileAssemblyWithResources
            "ManifestResourceTestAssembly"
            OutputKind.DynamicallyLinkedLibrary
            []
            resources
            [ source ]

    let private compileResourceExecutableImage (resources : ResourceDescription list) (source : string) : byte[] =
        Roslyn.compileAssemblyWithResources
            "ManifestResourceExecutable"
            OutputKind.ConsoleApplication
            []
            resources
            [ source ]

    let private prepareResourceExecutable
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (sourceName : string)
        (image : byte[])
        (implementations : WoofWare.PawPrint.ExternImplementations.ISystem_Environment_Env)
        : Program.PreparedProgram
        =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some sourceName) peImage dotnetRuntimes implementations [] with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected program to be ready before Main, but got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    let private assemblyNativeGetResourceMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let runtimeAssemblyType =
            requiredTopLevelType baseClassTypes.Corelib "System.Reflection" "RuntimeAssembly"

        let rawMethod =
            runtimeAssemblyType.Methods
            |> List.filter (fun method ->
                match method.NativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "AssemblyNative_GetResource"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point AssemblyNative_GetResource not found on RuntimeAssembly"
                | methods ->
                    failwith
                        $"QCall entry point AssemblyNative_GetResource was ambiguous on RuntimeAssembly: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, runtimeAssemblyType, method

    let private qCallAssemblyValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyFullName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallAssemblyType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallAssembly"

        let state, qCallAssemblyHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (qCallAssemblyType.Identity, SignatureTypeKind.ValueType))

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes qCallAssemblyHandle

        let value =
            match zero with
            | CliType.ValueType vt ->
                let assemblyField =
                    IlMachineState.requiredOwnInstanceFieldId state qCallAssemblyHandle "_assembly"

                CliValueType.WithFieldSetById
                    assemblyField
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)))
                    vt
                |> CliType.ValueType
            | other -> failwith $"QCallAssembly zero value was not a value type: %O{other}"

        value, state

    let private allocateNullTerminatedUtf16
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (value : string)
        : ManagedPointerSource * IlMachineState
        =
        let charHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Char

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero charHandle)
                (fun () -> CliType.ofChar (char 0))
                (value.Length + 1)
                state

        let mutable state = state

        for i = 0 to value.Length - 1 do
            state <- IlMachineState.setArrayValue arrayAddr (CliType.ofChar value.[i]) i state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private allocateUInt32Out
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let uint32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.UInt32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero uint32Handle)
                (fun () -> NativeCall.cliUInt32 0u)
                1
                state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private readUInt32Out (state : IlMachineState) (ptr : ManagedPointerSource) : uint32 =
        match IlMachineState.readManagedByref state ptr |> CliType.unwrapPrimitiveLikeDeep with
        | CliType.Numeric (CliNumericType.Int32 value) -> uint32 value
        | other -> failwith $"expected UInt32 out value, got %O{other}"

    let private readByte (state : IlMachineState) (ptr : ManagedPointerSource) (byteOffset : int) : byte =
        let ptr =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset byteOffset)

        match IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.UInt8 0uy)) with
        | CliType.Numeric (CliNumericType.UInt8 value) -> value
        | other -> failwith $"expected byte read, got %O{other}"

    let private readZeroBytes
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : byte[]
        =
        let zeroSizeDeclaredType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        let zeroSizeTemplate =
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                zeroSizeDeclaredType
                (Layout.Custom (size = 0, packingSize = 0))
                []
            |> CliType.ValueType

        IlMachineState.readManagedByrefBytesAs state ptr zeroSizeTemplate
        |> CliType.ToBytes

    let private invokeAssemblyNativeGetResource
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (implementations : WoofWare.PawPrint.ExternImplementations.ISystem_Environment_Env)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (resourceName : string)
        : IlMachineState * uint32 * EvalStackValue
        =
        let baseClassTypes = prepared.BaseClassTypes
        let sourceAssembly = state.ActiveAssembly prepared.EntryThread

        let state, runtimeAssemblyType, qCallMethod =
            assemblyNativeGetResourceMethod loggerFactory baseClassTypes state

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes sourceAssembly.Name.FullName state

        let resourceNamePtr, state =
            allocateNullTerminatedUtf16 baseClassTypes state resourceName

        let lengthOut, state = allocateUInt32Out baseClassTypes state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = qCallMethod
                Arguments =
                    ImmutableArray.CreateRange
                        [
                            qCallAssembly
                            CliType.RuntimePointer (CliRuntimePointer.Managed resourceNamePtr)
                            CliType.RuntimePointer (CliRuntimePointer.Managed lengthOut)
                        ]
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                Implementations = implementations
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = runtimeAssemblyType
            }

        let state =
            match NativeRuntimeAssembly.tryExecuteQCall "AssemblyNative_GetResource" ctx with
            | Some (ExecutionResult.Stepped (state, WhatWeDid.Executed)) -> state
            | Some result -> failwith $"unexpected AssemblyNative_GetResource execution result: %O{result}"
            | None -> failwith "AssemblyNative_GetResource QCall did not match"

        let returnValue, state = IlMachineState.popEvalStack prepared.EntryThread state
        state, readUInt32Out state lengthOut, returnValue

    let private compileManifestResourceMetadataAssemblyImage
        (resourceName : string)
        (resourceOffset : uint32)
        (managedResources : BlobBuilder)
        (implementation : MetadataBuilder -> EntityHandle)
        : byte[]
        =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "ForwardedResourceManifest.dll",
            metadata.GetOrAddGuid (Guid "4fb10f4e-7a5d-4f97-9d61-bac31ae93012"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "ForwardedResourceManifest",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        metadata.AddManifestResource (
            ManifestResourceAttributes.Public,
            metadata.GetOrAddString resourceName,
            implementation metadata,
            resourceOffset
        )
        |> ignore<ManifestResourceHandle>

        let metadataRoot = MetadataRootBuilder metadata
        let ilStream = BlobBuilder ()

        let peHeader =
            PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll))

        let peBuilder =
            ManagedPEBuilder (
                peHeader,
                metadataRoot,
                ilStream,
                null,
                managedResources,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    let private compileMetadataOnlyManifestResourceAssemblyImage
        (resourceName : string)
        (resourceOffset : uint32)
        (implementation : MetadataBuilder -> EntityHandle)
        : byte[]
        =
        compileManifestResourceMetadataAssemblyImage resourceName resourceOffset null implementation

    let private compileForwardedResourceAssemblyImageWithOffset
        (resourceName : string)
        (referencedAssemblyName : string)
        (resourceOffset : uint32)
        : byte[]
        =
        compileMetadataOnlyManifestResourceAssemblyImage
            resourceName
            resourceOffset
            (fun metadata ->
                let assemblyReferenceHandle =
                    metadata.AddAssemblyReference (
                        metadata.GetOrAddString referencedAssemblyName,
                        Version (2, 3, 4, 5),
                        Unchecked.defaultof<StringHandle>,
                        Unchecked.defaultof<BlobHandle>,
                        Unchecked.defaultof<AssemblyFlags>,
                        Unchecked.defaultof<BlobHandle>
                    )

                AssemblyReferenceHandle.op_Implicit assemblyReferenceHandle
            )

    let private compileForwardedResourceAssemblyImage
        (resourceName : string)
        (referencedAssemblyName : string)
        : byte[]
        =
        compileForwardedResourceAssemblyImageWithOffset resourceName referencedAssemblyName 0u

    let private compileInvalidLinkedResourceAssemblyImage (resourceName : string) : byte[] =
        compileMetadataOnlyManifestResourceAssemblyImage
            resourceName
            0u
            (fun _ -> MetadataTokens.EntityHandle (TableIndex.File, 1))

    let private compileInvalidForwardedResourceAssemblyImage (resourceName : string) : byte[] =
        compileMetadataOnlyManifestResourceAssemblyImage
            resourceName
            0u
            (fun _ -> MetadataTokens.EntityHandle (TableIndex.AssemblyRef, 1))

    let private compileUnsupportedImplementationResourceAssemblyImage (resourceName : string) : byte[] =
        compileMetadataOnlyManifestResourceAssemblyImage
            resourceName
            0u
            (fun _ -> MetadataTokens.EntityHandle (TableIndex.ExportedType, 1))

    let private compileEmbeddedResourceAssemblyImage
        (resourceName : string)
        (resourceOffset : uint32)
        (managedResources : BlobBuilder)
        : byte[]
        =
        compileManifestResourceMetadataAssemblyImage
            resourceName
            resourceOffset
            managedResources
            (fun _ -> Unchecked.defaultof<EntityHandle>)

    let private resourceDirectoryWithLengthPrefix (declaredPayloadLength : uint32) : BlobBuilder =
        let result = BlobBuilder ()
        result.WriteUInt32 declaredPayloadLength
        result

    [<Test>]
    let ``embedded manifest resource lookup returns exact payload metadata`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.TestData.bin"
        let resourceBytes = [| 0x10uy ; 0x20uy ; 0x30uy ; 0x40uy |]
        let emptyResourceName = "PawPrint.Empty.bin"

        let image =
            compileResourceAssemblyImage
                [
                    embeddedResource "PawPrint.Alpha.bin" [| 0x01uy |]
                    embeddedResource resourceName resourceBytes
                    embeddedResource emptyResourceName [||]
                ]

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName with
        | ManifestResourceLookupResult.Embedded resource ->
            resource.AssemblyFullName |> shouldEqual assembly.Name.FullName
            resource.Name |> shouldEqual resourceName
            resource.PayloadLength |> shouldEqual resourceBytes.Length

            let headerData =
                assembly.PeReader.GetSectionData (resource.PayloadRelativeVirtualAddress - 4)

            let mutable headerReader = headerData.GetReader ()

            headerReader.ReadUInt32 () |> shouldEqual (uint32 resourceBytes.Length)

            let sectionData =
                assembly.PeReader.GetSectionData resource.PayloadRelativeVirtualAddress

            let mutable reader = sectionData.GetReader ()
            reader.ReadBytes resourceBytes.Length |> shouldEqual resourceBytes
        | other -> failwith $"Expected embedded manifest resource, got %O{other}"

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly "PawPrint.Missing.bin" with
        | ManifestResourceLookupResult.NotFound -> ()
        | other -> failwith $"Expected missing manifest resource to be NotFound, got %O{other}"

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly "pawprint.testdata.bin" with
        | ManifestResourceLookupResult.NotFound -> ()
        | other -> failwith $"Expected case mismatch to be NotFound, got %O{other}"

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly emptyResourceName with
        | ManifestResourceLookupResult.Embedded resource ->
            resource.Name |> shouldEqual emptyResourceName
            resource.PayloadLength |> shouldEqual 0
        | other -> failwith $"Expected embedded empty manifest resource, got %O{other}"

    [<Test>]
    let ``embedded manifest resource with header outside resource directory reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.HeaderOutsideDirectory.bin"

        let image =
            compileEmbeddedResourceAssemblyImage resourceName 1u (resourceDirectoryWithLengthPrefix 0u)

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName

        ex.Message
        |> shouldContainText "header offset 1 is outside CLI resource directory size 4"

    [<Test>]
    let ``embedded manifest resource with payload outside resource directory reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.PayloadOutsideDirectory.bin"

        let image =
            compileEmbeddedResourceAssemblyImage resourceName 0u (resourceDirectoryWithLengthPrefix 8u)

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName
        ex.Message |> shouldContainText "declares payload end offset 12"
        ex.Message |> shouldContainText "beyond CLI resource directory size 4"

    [<Test>]
    let ``linked manifest resource lookup returns file metadata`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.LinkedPayload.bin"
        let fileName = "LinkedPayload.resources"

        let image =
            compileResourceAssemblyImage [ linkedResource resourceName fileName [| 0x11uy ; 0x22uy ; 0x33uy |] ]

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName with
        | ManifestResourceLookupResult.ExternalFile resource ->
            resource.AssemblyFullName |> shouldEqual assembly.Name.FullName
            resource.Name |> shouldEqual resourceName
            resource.FileName |> shouldEqual fileName
            resource.Offset |> shouldEqual 0L
        | other -> failwith $"Expected linked manifest resource file, got %O{other}"

    [<Test>]
    let ``linked manifest resource with non-existent File row reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.InvalidLinkedPayload.bin"
        let image = compileInvalidLinkedResourceAssemblyImage resourceName

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName
        ex.Message |> shouldContainText "invalid File row 1"

    [<Test>]
    let ``forwarded manifest resource with non-existent AssemblyRef row reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.InvalidForwardedPayload.bin"
        let image = compileInvalidForwardedResourceAssemblyImage resourceName

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName
        ex.Message |> shouldContainText "invalid AssemblyRef row 1"

    [<Test>]
    let ``manifest resource with unsupported implementation kind reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.UnsupportedImplementationPayload.bin"
        let image = compileUnsupportedImplementationResourceAssemblyImage resourceName

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName

        ex.Message
        |> shouldContainText "unsupported implementation handle kind ExportedType"

    [<Test>]
    let ``forwarded manifest resource with non-zero offset reports resource name`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.ForwardedPayloadWithOffset.bin"
        let referencedAssemblyName = "PawPrint.ResourceCarrier"

        let image =
            compileForwardedResourceAssemblyImageWithOffset resourceName referencedAssemblyName 4u

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName
                |> ignore<ManifestResourceLookupResult>
            )

        ex.Message |> shouldContainText resourceName
        ex.Message |> shouldContainText "declares non-zero offset 4"

    [<Test>]
    let ``forwarded manifest resource lookup returns assembly reference metadata`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.ForwardedPayload.bin"
        let referencedAssemblyName = "PawPrint.ResourceCarrier"

        let image =
            compileForwardedResourceAssemblyImage resourceName referencedAssemblyName

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName with
        | ManifestResourceLookupResult.ReferencedAssembly (actualResourceName, assemblyReference) ->
            actualResourceName |> shouldEqual resourceName
            assemblyReference.Name.Name |> shouldEqual referencedAssemblyName
            assemblyReference.Version |> shouldEqual (Version (2, 3, 4, 5))
        | other -> failwith $"Expected forwarded manifest resource assembly reference, got %O{other}"

    [<Test>]
    let ``embedded manifest resource can be projected as PE byte range`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let resourceName = "PawPrint.Payload.bin"
        let emptyResourceName = "PawPrint.EmptyPayload.bin"
        let resourceBytes = [| 0xCAuy ; 0xFEuy ; 0xBAuy ; 0xBEuy ; 0x01uy |]

        let image =
            compileResourceAssemblyImage
                [
                    embeddedResource resourceName resourceBytes
                    embeddedResource emptyResourceName [||]
                ]

        use assemblyStream = new MemoryStream (image)

        use assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let resource =
            match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly resourceName with
            | ManifestResourceLookupResult.Embedded resource -> resource
            | other -> failwith $"Expected embedded manifest resource, got %O{other}"

        let emptyResource =
            match global.WoofWare.PawPrint.AssemblyApi.findManifestResource assembly emptyResourceName with
            | ManifestResourceLookupResult.Embedded resource -> resource
            | other -> failwith $"Expected embedded empty manifest resource, got %O{other}"

        let peByteRange = IlMachineState.peByteRangeForEmbeddedManifestResource resource

        peByteRange.AssemblyFullName |> shouldEqual assembly.Name.FullName

        peByteRange.Source
        |> shouldEqual (PeByteRangePointerSource.ManagedResource resourceName)

        peByteRange.RelativeVirtualAddress
        |> shouldEqual resource.PayloadRelativeVirtualAddress

        peByteRange.Size |> shouldEqual resourceBytes.Length

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

        let baseClassTypes = Corelib.getBaseTypes corelib

        let state =
            IlMachineState.initial loggerFactory ImmutableArray.Empty assembly
            |> fun state -> state.WithLoadedAssembly corelib.Name corelib

        let state, ptr =
            IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

        ManagedPointerSource.tryStableAddressBits ptr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress))

        let byteTemplate = CliType.Numeric (CliNumericType.UInt8 0uy)

        IlMachineState.readManagedByrefBytesAs state ptr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 resourceBytes.[0]))

        let offsetPtr =
            ptr |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 3)

        ManagedPointerSource.tryStableAddressBits offsetPtr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress + 3L))

        IlMachineState.readManagedByrefBytesAs state offsetPtr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 resourceBytes.[3]))

        let emptyPeByteRange =
            IlMachineState.peByteRangeForEmbeddedManifestResource emptyResource

        emptyPeByteRange.Size |> shouldEqual 0

        let state, emptyPtr =
            IlMachineState.peByteRangePointer loggerFactory baseClassTypes emptyPeByteRange state

        readZeroBytes baseClassTypes state emptyPtr |> shouldEqual [||]

    [<Test>]
    let ``AssemblyNative_GetResource returns embedded resource byte range`` () : unit =
        let resourceName =
            "PawPrint.ManagedStreamPayload."
            + System.String [| char 0xD83D ; char 0xDE00 |]
            + ".bin"

        let emptyResourceName = "PawPrint.EmptyManagedStreamPayload.bin"
        let resourceBytes = [| 0xCAuy ; 0xFEuy ; 0xBAuy ; 0xBEuy ; 0x01uy |]

        let source =
            """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

        let image =
            compileResourceExecutableImage
                [
                    embeddedResource resourceName resourceBytes
                    embeddedResource emptyResourceName [||]
                ]
                source

        let _, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "AssemblyNativeGetResource.cs" ]

        use _loggerFactoryResource = loggerFactory

        let mockEnv = MockEnv.make ()

        let prepared =
            prepareResourceExecutable loggerFactory "AssemblyNativeGetResource.cs" image mockEnv

        let state, missingLength, missingReturn =
            invokeAssemblyNativeGetResource loggerFactory mockEnv prepared prepared.State "PawPrint.MissingPayload.bin"

        missingLength |> shouldEqual 0u

        missingReturn
        |> shouldEqual (EvalStackValue.ManagedPointer ManagedPointerSource.Null)

        let state, emptyLength, emptyReturn =
            invokeAssemblyNativeGetResource loggerFactory mockEnv prepared state emptyResourceName

        emptyLength |> shouldEqual 0u

        match emptyReturn with
        | EvalStackValue.ManagedPointer ManagedPointerSource.Null ->
            failwith "expected empty manifest resource to return a non-null PE byte-range pointer"
        | EvalStackValue.ManagedPointer ptr -> readZeroBytes prepared.BaseClassTypes state ptr |> shouldEqual [||]
        | other -> failwith $"expected managed resource pointer for empty manifest resource, got %O{other}"

        let state, length, ret =
            invokeAssemblyNativeGetResource loggerFactory mockEnv prepared state resourceName

        length |> shouldEqual (uint32 resourceBytes.Length)

        match ret with
        | EvalStackValue.ManagedPointer ptr ->
            resourceBytes
            |> Array.iteri (fun i expected -> readByte state ptr i |> shouldEqual expected)
        | other -> failwith $"expected managed resource pointer, got %O{other}"
