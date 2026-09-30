namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

/// The identity a dynamic assembly is created with: the fields of the `NativeAssemblyNameParts` that
/// `RuntimeAssemblyBuilder.CreateDynamicAssembly` fills from an `AssemblyName`, which CoreCLR hands
/// unchanged to `IMetaDataAssemblyEmit::DefineAssembly`.
type DynamicAssemblyName =
    {
        /// Never empty: `Assembly::CreateDynamic` refuses an empty name before defining anything.
        SimpleName : string
        Version : Version
        /// Empty for the neutral culture, whether the caller passed no culture or an empty one.
        Culture : string
        /// The full public key, or empty. `RuntimeAssemblyBuilder` passes `AssemblyName.GetPublicKey`,
        /// never a token.
        PublicKey : ImmutableArray<byte>
        /// `AssemblyName.RawFlags`, stored verbatim (measured: `Retargetable` and
        /// `EnableJITcompileTracking` both survive into the assembly's name).
        Flags : AssemblyFlags
        /// Already defaulted: CoreCLR stores SHA1 when the caller passes zero.
        HashAlgorithm : AssemblyHashAlgorithm
    }

/// A dynamic assembly's metadata as an ECMA-335 image, which is how PawPrint gives an assembly that has
/// no file the same standing as one loaded from disk. CoreCLR's `Assembly::CreateDynamic` does the same
/// in memory: it defines the `Assembly` row in a fresh metadata emit scope and builds the assembly over
/// that, so a dynamic assembly there has metadata too.
[<RequireQualifiedAccess>]
module DynamicAssemblyImage =
    /// The `Module` row's name, which is what `Module.ScopeName` reports for every dynamic manifest
    /// module (measured on .NET 10).
    let manifestModuleName : string = "RefEmit_InMemoryManifestModule"

    /// The image holding exactly what CoreCLR's emit scope holds when `CreateDynamic` returns: the
    /// `Module` row with the given version ID, the `Assembly` row, and the `<Module>` type that owns
    /// module-scope members. No references, no other types, no resources and no entry point.
    ///
    /// A function of its arguments alone, so a replay rebuilds the same bytes.
    let build (name : DynamicAssemblyName) (moduleVersionId : Guid) : byte[] =
        if String.IsNullOrEmpty name.SimpleName then
            invalidArg (nameof name) "a dynamic assembly's simple name must be non-empty"

        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString manifestModuleName,
            metadata.GetOrAddGuid moduleVersionId,
            GuidHandle (),
            GuidHandle ()
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString name.SimpleName,
            name.Version,
            (if name.Culture = "" then
                 StringHandle ()
             else
                 metadata.GetOrAddString name.Culture),
            (if name.PublicKey.IsEmpty then
                 BlobHandle ()
             else
                 metadata.GetOrAddBlob name.PublicKey),
            name.Flags,
            name.HashAlgorithm
        )
        |> ignore<AssemblyDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.NotPublic,
            StringHandle (),
            metadata.GetOrAddString "<Module>",
            EntityHandle (),
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder.CreateLibraryHeader (),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                deterministicIdProvider = (fun _ -> BlobContentId (Guid.Empty, 0u))
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray ()
