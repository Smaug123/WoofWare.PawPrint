namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped

/// CoreLib images that state a different `AssemblyVersion` major from the host's, for tests of
/// what refuses a CoreLib by its major.
[<RequireQualifiedAccess>]
module PatchedCoreLib =

    /// A copy of the host's CoreLib whose AssemblyDef row states `major`, written into `dir`.
    ///
    /// Assembly binding is by simple name and ignores the referenced version, so a directory
    /// holding this at the head of `DotnetRuntimeDirs` is where the guest's CoreLib comes from.
    let write (major : uint16) (dir : string) : string =
        let bytes = File.ReadAllBytes typeof<obj>.Assembly.Location

        let offset =
            use peReader = new PEReader (ImmutableArray.Create<byte> bytes)
            let metadata = peReader.GetMetadataReader ()
            // ECMA-335 II.22.2: an Assembly row is HashAlgId (4 bytes), then MajorVersion (2 bytes).
            peReader.PEHeaders.MetadataStartOffset
            + metadata.GetTableMetadataOffset TableIndex.Assembly
            + 4

        // The offset is right if it finds the major the unpatched image states.
        BinaryPrimitives.ReadUInt16LittleEndian (ReadOnlySpan (bytes, offset, 2))
        |> int
        |> shouldEqual (typeof<obj>.Assembly.GetName().Version.Major)

        BinaryPrimitives.WriteUInt16LittleEndian (Span (bytes, offset, 2), major)

        let path = Path.Combine (dir, "System.Private.CoreLib.dll")
        File.WriteAllBytes (path, bytes)

        use peReader = new PEReader (ImmutableArray.Create<byte> bytes)

        let patchedDefinition = peReader.GetMetadataReader().GetAssemblyDefinition ()

        patchedDefinition.Version.Major |> shouldEqual (int major)

        path
