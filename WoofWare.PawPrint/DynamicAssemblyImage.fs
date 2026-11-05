namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection

/// The identity a dynamic assembly is created with: the fields of the `NativeAssemblyNameParts` that
/// `RuntimeAssemblyBuilder.CreateDynamicAssembly` fills from an `AssemblyName`, which CoreCLR hands
/// unchanged to `IMetaDataAssemblyEmit::DefineAssembly`.
type DynamicAssemblyName =
    {
        /// Never empty: `Assembly::CreateDynamic` refuses an empty name before defining anything.
        SimpleName : string
        /// Already defaulted: CoreCLR stores 0 for a component the caller passes as 65535.
        Version : Version
        /// Empty for the neutral culture, whether the caller passed no culture or an empty one.
        Culture : string
        /// The full public key, or empty. `RuntimeAssemblyBuilder` passes `AssemblyName.GetPublicKey`,
        /// never a token.
        PublicKey : ImmutableArray<byte>
        /// `AssemblyName.RawFlags`, stored verbatim, all thirty-two bits (measured: `Retargetable` and
        /// `EnableJITcompileTracking` both survive into the assembly's name, and so do bits above the sixteen ECMA-335 defines).
        Flags : AssemblyFlags
        /// Already defaulted: CoreCLR stores SHA1 when the caller passes zero, and nothing when it
        /// passes all ones.
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

    /// The emit scope holding exactly what CoreCLR's holds when `CreateDynamic` returns: the `Module` row
    /// with the given version ID, the `<Module>` type that owns module-scope members, and the `Assembly`
    /// row. No references, no other types, no resources and no entry point.
    let scope (name : DynamicAssemblyName) (moduleVersionId : Guid) : EmitScope =
        if String.IsNullOrEmpty name.SimpleName then
            invalidArg (nameof name) "a dynamic assembly's simple name must be non-empty"

        let versionComponent (value : int) : uint16 =
            if value < 0 || value > 0xFFFF then
                invalidArg (nameof name) $"version %O{name.Version} has a component outside 0 to 65535"

            uint16 value

        let version : MetadataVersion =
            {
                Major = versionComponent name.Version.Major
                Minor = versionComponent name.Version.Minor
                Build = versionComponent name.Version.Build
                Revision = versionComponent name.Version.Revision
            }

        let scope = EmitScope.create manifestModuleName moduleVersionId
        let simpleName, scope = EmitScope.internString name.SimpleName scope
        let culture, scope = EmitScope.internString name.Culture scope
        let publicKey, scope = EmitScope.internBlob name.PublicKey scope

        EmitScope.defineAssembly
            {
                HashAlgorithm = name.HashAlgorithm
                Version = version
                Flags = name.Flags
                PublicKey = publicKey
                Name = simpleName
                Culture = culture
            }
            scope

    /// The image of `scope`. A function of its arguments alone, so a replay rebuilds the same bytes.
    let build (name : DynamicAssemblyName) (moduleVersionId : Guid) : byte[] =
        scope name moduleVersionId |> EmitScopeImage.write
