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

        IlMachineState.readManagedByrefBytesAs state emptyPtr zeroSizeTemplate
        |> CliType.ToBytes
        |> shouldEqual [||]
