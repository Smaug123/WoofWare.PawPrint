namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// A MethodSpec's blob opens with the byte GENERICINST (0x0a), and with no other: CoreCLR compares
/// the whole byte (memberload.cpp, GetMethodDescFromMethodSpec), so GENERICINST with a flag such as
/// HASTHIS set is refused as surely as another calling convention. Checked for every byte against
/// <c>Module.ResolveMethod</c>.
/// </summary>
/// <remarks>
/// CoreCLR loads such an image, and refuses the MethodSpec with <c>BadImageFormatException</c> only
/// when it is used; PawPrint refuses the image when it reads it. Both refuse the blob, which is what
/// this checks.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMethodSpecHeader =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    /// An image whose class `Calls` declares `static void M<T>()`, and whose one MethodSpec names `M`
    /// with the blob `header; 1; int32`. Returns the image and the MethodSpec's token.
    let private emit (name : string) (header : byte) : byte[] * int =
        let md = MetadataBuilder ()
        let text (s : string) : StringHandle = md.GetOrAddString s

        md.AddModule (0, text name, md.GetOrAddGuid (Guid.NewGuid ()), GuidHandle (), GuidHandle ())
        |> ignore

        md.AddAssembly (
            text name,
            Version (1, 0, 0, 0),
            StringHandle (),
            BlobHandle (),
            enum 0,
            AssemblyHashAlgorithm.None
        )
        |> ignore

        let coreName = typeof<obj>.Assembly.GetName ()

        let coreRef =
            md.AddAssemblyReference (
                text coreName.Name,
                coreName.Version,
                StringHandle (),
                md.GetOrAddBlob (coreName.GetPublicKeyToken ()),
                enum 0,
                BlobHandle ()
            )

        let objectRef =
            md.AddTypeReference (AssemblyReferenceHandle.op_Implicit coreRef, text "System", text "Object")

        md.AddTypeDefinition (
            TypeAttributes.NotPublic,
            StringHandle (),
            text "<Module>",
            EntityHandle (),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore

        md.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            StringHandle (),
            text "Calls",
            TypeReferenceHandle.op_Implicit objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore

        let bodies = BlobBuilder ()
        let il = InstructionEncoder (BlobBuilder ())
        il.OpCode ILOpCode.Ret

        let m =
            md.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                text "M",
                // A generic method of one parameter, returning void and taking nothing.
                md.GetOrAddBlob [| 0x10uy ; 1uy ; 0uy ; 0x01uy |],
                (MethodBodyStreamEncoder bodies).AddMethodBody il,
                MetadataTokens.ParameterHandle 1
            )

        md.AddGenericParameter (MethodDefinitionHandle.op_Implicit m, GenericParameterAttributes.None, text "T", 0)
        |> ignore

        let spec =
            md.AddMethodSpecification (
                MethodDefinitionHandle.op_Implicit m,
                md.GetOrAddBlob [| header ; 1uy ; 0x08uy |]
            )

        let pe =
            ManagedPEBuilder (PEHeaderBuilder.CreateLibraryHeader (), MetadataRootBuilder md, bodies)

        let blob = BlobBuilder ()
        pe.Serialize blob |> ignore
        blob.ToArray (), MetadataTokens.GetToken (MethodSpecificationHandle.op_Implicit spec : EntityHandle)

    /// Whether the host refuses the MethodSpec when it resolves it.
    let private hostRefuses (name : string) (image : byte[]) (token : int) : bool =
        let context = AssemblyLoadContext (name, isCollectible = true)

        try
            use stream = new MemoryStream (image)
            let host = (context.LoadFromStream stream).ManifestModule

            try
                host.ResolveMethod token |> ignore<MethodBase>
                false
            with :? ArgumentException as e when (e.InnerException :? BadImageFormatException) ->
                true
        finally
            context.Unload ()

    /// Whether PawPrint refuses the MethodSpec when it reads the image.
    let private pawPrintRefuses (name : string) (image : byte[]) : bool =
        use stream = new MemoryStream (image)

        try
            Assembly.read loggerFactory (Some (name + ".dll")) stream
            |> ignore<DumpedAssembly>

            false
        with :? BadImageFormatException ->
            true

    [<Test>]
    let ``Only GENERICINST opens a MethodSpec, for PawPrint as for CoreCLR`` () : unit =
        for header in 0uy .. 255uy do
            let name = $"MethodSpecHeader%02x{header}"
            let image, token = emit name header
            let host = hostRefuses name image token

            host |> shouldEqual (header <> 0x0auy)

            if pawPrintRefuses name image <> host then
                failwith $"header 0x%02x{header}: the host refuses it: %b{host}; PawPrint disagrees"
