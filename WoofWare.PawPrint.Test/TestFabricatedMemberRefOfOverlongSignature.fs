namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Two generic methods `D.M<T>(T)` whose signatures differ only in how they encode `T`'s index: in its
/// shortest form, and in an overlong two bytes, which no compiler writes and CoreCLR reads all the same.
/// CoreCLR's emitter writes the index in its shortest form when it imports a signature into a dynamic
/// module, so `ModuleBuilder.GetMethodMetadataToken` gives both methods one `MemberRef`, as measured on
/// .NET 10.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedMemberRefOfOverlongSignature =

    let private fabricatedName : string = "OverlongSignature"

    let private signatures : byte[] list =
        [
            [| 0x30uy ; 0x01uy ; 0x01uy ; 0x01uy ; 0x1Euy ; 0x00uy |]
            [| 0x30uy ; 0x01uy ; 0x01uy ; 0x01uy ; 0x1Euy ; 0x80uy ; 0x00uy |]
        ]

    /// An image with an abstract class `D` declaring an abstract `M<T>(T)` for each signature.
    let private fabricate () : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString $"%s{fabricatedName}.dll",
            metadata.GetOrAddGuid Guid.Empty,
            GuidHandle (),
            GuidHandle ()
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString fabricatedName,
            Version (1, 0, 0, 0),
            StringHandle (),
            BlobHandle (),
            enum<AssemblyFlags> 0,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let coreLib = typeof<obj>.Assembly.GetName ()

        let coreLibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString coreLib.Name,
                coreLib.Version,
                StringHandle (),
                metadata.GetOrAddBlob (coreLib.GetPublicKeyToken ()),
                enum<AssemblyFlags> 0,
                BlobHandle ()
            )

        let objectRef =
            metadata.AddTypeReference (
                AssemblyReferenceHandle.op_Implicit coreLibRef,
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        metadata.AddTypeDefinition (
            enum<TypeAttributes> 0,
            StringHandle (),
            metadata.GetOrAddString "<Module>",
            EntityHandle (),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Class,
            StringHandle (),
            metadata.GetOrAddString "D",
            TypeReferenceHandle.op_Implicit objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let methods =
            [
                for signature in signatures ->
                    metadata.AddMethodDefinition (
                        MethodAttributes.Public
                        ||| MethodAttributes.Virtual
                        ||| MethodAttributes.Abstract
                        ||| MethodAttributes.HideBySig
                        ||| MethodAttributes.NewSlot,
                        MethodImplAttributes.IL,
                        metadata.GetOrAddString "M",
                        metadata.GetOrAddBlob signature,
                        -1,
                        MetadataTokens.ParameterHandle 1
                    )
            ]

        // Owners in method order, which is the GenericParam table's sort order here.
        for m in methods do
            metadata.AddGenericParameter (
                MethodDefinitionHandle.op_Implicit m,
                GenericParameterAttributes.None,
                metadata.GetOrAddString "T",
                0
            )
            |> ignore<GenericParameterHandle>

        let image = BlobBuilder ()

        ManagedPEBuilder(
            PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
            MetadataRootBuilder metadata,
            BlobBuilder ()
        )
            .Serialize
            image
        |> ignore<BlobContentId>

        image.ToArray ()

    let private driver : string =
        """
using System;
using System.Reflection;
using System.Reflection.Emit;

public static class Program
{
    public static int Main()
    {
        ModuleBuilder module = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("Dyn"), AssemblyBuilderAccess.Run).DefineDynamicModule("Dyn");
        MethodInfo[] methods = typeof(D).GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);

        if (methods.Length != 2)
        {
            return 1;
        }

        if (module.GetMethodMetadataToken(methods[0]) != 0x0A000001)
        {
            return 2;
        }

        if (module.GetMethodMetadataToken(methods[1]) != 0x0A000001)
        {
            return 3;
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``two encodings of one signature are one member reference`` () : unit =
        let appContext =
            AppContextProperties.ofMap (
                Map.ofList
                    [
                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                    ]
            )

        let onHost, onPawPrint =
            FabricatedGuest.runOnBothUnder appContext fabricatedName (fabricate ()) "Driver" driver

        onHost |> shouldEqual (RealRuntimeResult.NormalExit 0)
        onPawPrint |> shouldEqual (FabricatedOutcome.Exited 0)
