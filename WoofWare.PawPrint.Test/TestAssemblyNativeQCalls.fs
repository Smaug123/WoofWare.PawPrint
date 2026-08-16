namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Tests for the QCalls behind `Assembly.GetName()`. Most are answered from a column of the
/// manifest's single `Assembly` metadata row; the `ModuleHandle_*` ones are keyed by the
/// manifest module instead and read the image's headers rather than that row.
///
/// There is no end-to-end guest coverage yet: `Assembly.GetName()` is the only managed
/// caller a guest can reach, and it needs nine runtime primitives in sequence, so it stays
/// parked until the last of them lands (see `sourcesPure/AssemblyGetNameSimpleName.cs`,
/// whose comment enumerates them). These tests pin each one as it arrives.
[<TestFixture>]
module TestAssemblyNativeQCalls =

    /// Deliberately dotted. CoreCLR reads the simple name straight out of metadata, so a
    /// dotted name comes back whole; an implementation that split a qualified name at a
    /// '.' (as the type-name QCalls legitimately do) would truncate it.
    let private guestAssemblyName = "WoofWare.PawPrint.SimpleNameTestGuest"

    /// Four distinct, non-zero components, so a handler that transposed two of them or
    /// wrote one pointer four times cannot pass. Deliberately not Roslyn's `0.0.0.0`
    /// default, which every transposition survives.
    let private guestAssemblyVersion = System.Version (4, 3, 2, 1)

    /// Seeded into every `out int` slot before a call. Negative, so it cannot collide with
    /// any value CoreCLR can write through one of these pointers (the metadata columns it
    /// widens are `USHORT`), which makes "never written" a distinguishable outcome.
    let private unwrittenSentinel = -1

    let private guestSource =
        """
[assembly: System.Reflection.AssemblyVersion("4.3.2.1")]

public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    /// The oracle for both QCalls below: the `Assembly` row read from the same image with
    /// `MetadataReader` — i.e. exactly what CoreCLR's `GetAssemblyProps(TokenFromRid(1,
    /// mdtAssembly), ...)` hands back. Reading it from the image rather than restating the
    /// constants keeps the test honest if the compiler ever mangles what it was given.
    let private metadataAssemblyDefinition (image : byte[]) : string * System.Version =
        use peImage = new MemoryStream (image)
        use peReader = new System.Reflection.PortableExecutable.PEReader (peImage)
        let metadata = peReader.GetMetadataReader ()
        let assemblyDef = metadata.GetAssemblyDefinition ()
        metadata.GetString assemblyDef.Name, assemblyDef.Version

    let private metadataAssemblyName (image : byte[]) : string = metadataAssemblyDefinition image |> fst

    /// A culture on the manifest row is only legal for a library — the C# compiler rejects
    /// `[assembly: AssemblyCulture]` on an executable (CS7059, "executables cannot be
    /// satellite assemblies"). So a cultured assembly has to be compiled separately and
    /// loaded alongside the guest.
    let private culturedAssemblyName = "WoofWare.PawPrint.CulturedTestLibrary"

    /// Deliberately not the canonical casing: `AssemblyName.CultureName` normalises this
    /// through `CultureInfo` and reports `en-GB`, whereas CoreCLR's `md.szLocale` — and so
    /// the string the guest receives — is the column verbatim.
    let private culturedColumn = "EN-gb"

    let private culturedNormalised = "en-GB"

    let private culturedLibraryImage () : byte[] =
        let source =
            $"""
[assembly: System.Reflection.AssemblyCulture("{culturedColumn}")]

public static class Satellite
{{
    public static int Value => 1;
}}
"""

        Roslyn.compileAssembly culturedAssemblyName OutputKind.DynamicallyLinkedLibrary [] [ source ]

    /// A manifest `Flags` column carrying a bit that `AssemblyName.Flags` masks away. Its
    /// getter returns `_flags &&& 0xFFFFF10F`, so the ContentType bits (`0x0E00`) vanish
    /// there while CoreCLR's QCall — and `AssemblyName.RawFlags`, which the caller assigns
    /// to — keep them. `0x200` is WindowsRuntime; the C# compiler emits whatever
    /// `[assembly: AssemblyFlags]` is given, masked view or not.
    let private maskedFlagsAssemblyName = "WoofWare.PawPrint.MaskedFlagsTestLibrary"

    let private maskedFlagsColumn = 0x200

    let private maskedFlagsLibraryImage () : byte[] =
        let source =
            $"""
[assembly: System.Reflection.AssemblyFlags(0x{maskedFlagsColumn:x}u)]

public static class MaskedFlags
{{
    public static int Value => 1;
}}
"""

        Roslyn.compileAssembly maskedFlagsAssemblyName OutputKind.DynamicallyLinkedLibrary [] [ source ]

    /// Every image Roslyn emits carries SHA1 (`0x8004`) in its `HashAlgId` column unless
    /// told otherwise, and corelib is SHA1 too, so without an assembly that says something
    /// else nothing here could tell a handler that reads the column from one that returns a
    /// constant. `[assembly: AssemblyAlgorithmId]` writes the column directly.
    ///
    /// The value is deliberately not an `AssemblyHashAlgorithm` case. ECMA-335 II.23.1.1
    /// makes the column a `ULONG` and constrains it no further, CoreCLR's `GetAssemblyProps`
    /// hands it back unexamined, and the managed caller casts it to the enum without
    /// validating — so an unnamed value is a legal thing for a guest to see, and pinning it
    /// down here also rules out a handler that tried to canonicalise through the enum.
    let private hashAlgorithmAssemblyName = "WoofWare.PawPrint.HashAlgorithmTestLibrary"

    let private hashAlgorithmColumn = 0x1234

    let private hashAlgorithmLibraryImage () : byte[] =
        let source =
            $"""
[assembly: System.Reflection.AssemblyAlgorithmId(0x{hashAlgorithmColumn:x}u)]

public static class HashAlgorithmLibrary
{{
    public static int Value => 1;
}}
"""

        Roslyn.compileAssembly hashAlgorithmAssemblyName OutputKind.DynamicallyLinkedLibrary [] [ source ]

    /// A trivial library to patch a table stream version into. Each caller passes a distinct
    /// `suffix`, because assemblies are registered by full name and two same-named images
    /// would displace one another.
    let private peKindLibraryImage (suffix : string) : byte[] =
        Roslyn.compileAssembly
            $"WoofWare.PawPrint.PEKind%s{suffix}TestLibrary"
            OutputKind.DynamicallyLinkedLibrary
            []
            [
                """
public static class PEKindLibrary
{
    public static int Value => 1;
}
"""
            ]

    let private streamVersionLibraryImage (suffix : string) : byte[] =
        Roslyn.compileAssembly
            $"WoofWare.PawPrint.StreamVersion%s{suffix}TestLibrary"
            OutputKind.DynamicallyLinkedLibrary
            []
            [
                """
public static class StreamVersionLibrary
{
    public static int Value => 1;
}
"""
            ]

    /// Rewrites the `MajorVersion`/`MinorVersion` bytes of `image`'s metadata table stream
    /// header, returning a fresh image. Every toolchain emits 2.0 and there is no compiler
    /// switch for anything else, so patching the bytes is the only way to get a second
    /// answer — and a second answer is needed, because 2.0 is also what a handler that
    /// hardcoded `MD_STREAM_VER_2` would return.
    ///
    /// This is not a synthetic shape the runtime would reject: `MetadataReader` opens a 1.0
    /// table stream perfectly happily, which is exactly why the handler cannot assume 2.0.
    ///
    /// Walks ECMA-335 II.24.2.1's metadata root to find the stream header, then converts the
    /// root-relative stream offset to a file offset via `MetadataStartOffset`. Deliberately
    /// an independent parse rather than a call into `DumpedAssembly`, so it is an oracle for
    /// the member under test rather than a restatement of it.
    let private withTableStreamVersion (major : byte, minor : byte) (image : byte[]) : byte[] =
        use peImage = new MemoryStream (image)
        use peReader = new System.Reflection.PortableExecutable.PEReader (peImage)
        let metadataStart = peReader.PEHeaders.MetadataStartOffset
        let mutable reader = peReader.GetMetadata().GetReader ()

        reader.ReadUInt32 () |> ignore<uint32> // BSJB signature
        reader.ReadUInt16 () |> ignore<uint16> // root MajorVersion
        reader.ReadUInt16 () |> ignore<uint16> // root MinorVersion
        reader.ReadUInt32 () |> ignore<uint32> // Reserved
        let versionStringLength = reader.ReadInt32 ()
        reader.ReadBytes versionStringLength |> ignore<byte[]>
        reader.ReadUInt16 () |> ignore<uint16> // Flags
        let streamCount = int (reader.ReadUInt16 ())

        let patched = Array.copy image
        let mutable found = false

        for _ = 1 to streamCount do
            let offset = reader.ReadInt32 ()
            reader.ReadInt32 () |> ignore<int> // Size
            let nameStart = reader.Offset
            let mutable nameLength = 0

            while reader.ReadByte () <> 0uy do
                nameLength <- nameLength + 1

            reader.Offset <- nameStart
            let name = reader.ReadUTF8 nameLength
            reader.ReadByte () |> ignore<byte>
            reader.Align 4uy

            if name = "#~" || name = "#-" then
                // II.24.2.6: Reserved (4 bytes), MajorVersion, MinorVersion.
                patched.[metadataStart + offset + 4] <- major
                patched.[metadataStart + offset + 5] <- minor
                found <- true

        if not found then
            failwith "test fixture image has no #~ or #- metadata table stream to patch"

        patched

    /// The PE, COR and ReadyToRun header fields that decide an image's PE kind, read out of
    /// the raw bytes here rather than through `DumpedAssembly.PEImageHeaders`.
    ///
    /// This reports the *inputs*, never the kind: the expected kinds below stay written-down
    /// constants, because a second implementation of the packing could be wrong in the same
    /// way as the first. What it buys is that a test can assert what shape of image it is
    /// looking at — so "corelib answers ILOnly/I386" cannot silently become a vacuous claim
    /// if the framework ever stops shipping ReadyToRun images.
    ///
    /// The RVA-to-file-offset walk is done by hand off the section table, rather than through
    /// `PEReader.GetSectionData` as the member under test does, so the directory lookup and
    /// the field offsets are not shared with it.
    let private imageHeaderFacts
        (image : byte[])
        : {|
              Machine : Machine
              IsPE32Plus : bool
              CorFlags : CorFlags
              ReadyToRunFlags : uint32 option
          |}
        =
        use peImage = new MemoryStream (image)
        use peReader = new PEReader (peImage)
        let headers = peReader.PEHeaders

        let corHeaderStart = headers.CorHeaderStartOffset
        // IMAGE_COR20_HEADER: cb, MajorRuntimeVersion, MinorRuntimeVersion, MetaData
        // directory, then Flags at offset 16 and the ManagedNativeHeader directory at 64.
        let corFlags =
            System.BitConverter.ToInt32 (image, corHeaderStart + 16) |> enum<CorFlags>

        let nativeHeaderRva = System.BitConverter.ToInt32 (image, corHeaderStart + 64)
        let nativeHeaderSize = System.BitConverter.ToInt32 (image, corHeaderStart + 68)

        let rvaToOffset (rva : int) : int option =
            headers.SectionHeaders
            |> Seq.tryPick (fun section ->
                if
                    rva >= section.VirtualAddress
                    && rva < section.VirtualAddress + section.SizeOfRawData
                then
                    Some (section.PointerToRawData + (rva - section.VirtualAddress))
                else
                    None
            )

        let readyToRunFlags =
            // `sizeof(READYTORUN_HEADER)` is 16: Signature, MajorVersion, MinorVersion,
            // Flags, NumberOfSections.
            if nativeHeaderRva = 0 || nativeHeaderSize < 16 then
                None
            else
                match rvaToOffset nativeHeaderRva with
                | None -> None
                | Some offset ->
                    // READYTORUN_SIGNATURE, "RTR\0".
                    if System.BitConverter.ToUInt32 (image, offset) <> 0x00525452u then
                        None
                    else
                        System.BitConverter.ToUInt32 (image, offset + 8) |> Some

        {|
            Machine = headers.CoffHeader.Machine
            IsPE32Plus = headers.PEHeader.Magic = PEMagic.PE32Plus
            CorFlags = corFlags
            ReadyToRunFlags = readyToRunFlags
        |}

    /// Rewrites an image's COR header `Flags`, returning a fresh image. The 32-bit flags are
    /// the two bits `peKindAndMachine` reads that no compiler will emit on request here
    /// (Roslyn only sets them for `/platform:x86` and `anycpu32bitpreferred`, neither of
    /// which the test harness compiles with), so patching is how to reach those arms through
    /// a real image rather than through the pure function alone.
    let private withCorFlags (flags : CorFlags) (image : byte[]) : byte[] =
        use peImage = new MemoryStream (image)
        use peReader = new PEReader (peImage)
        let patched = Array.copy image
        let bytes = System.BitConverter.GetBytes (int flags)
        bytes.CopyTo (patched, peReader.PEHeaders.CorHeaderStartOffset + 16)
        patched

    /// Rewrites an image's COFF header `Machine`, returning a fresh image. Every image the
    /// harness compiles is AnyCPU and so says I386, which is also what the ReadyToRun
    /// platform-neutral arm substitutes — so without this, a handler that ignored the COFF
    /// header entirely would pass.
    let private withMachine (machine : Machine) (image : byte[]) : byte[] =
        use peImage = new MemoryStream (image)
        use peReader = new PEReader (peImage)
        let patched = Array.copy image
        let bytes = System.BitConverter.GetBytes (uint16 machine)
        bytes.CopyTo (patched, peReader.PEHeaders.CoffHeaderStartOffset)
        patched

    /// Writes a `READYTORUN_HEADER` into the image and points the COR header's
    /// `ManagedNativeHeader` directory at it, declaring `declaredSize` bytes. Returns a fresh
    /// image.
    ///
    /// No compiler under test emits a ReadyToRun image — crossgen2 does, and the framework
    /// assemblies PawPrint loads are its output, but those cannot be patched or rebuilt here.
    /// So this synthesises one, which is what lets the ReadyToRun arms be exercised through a
    /// real image rather than only through the decoder.
    ///
    /// The header goes in the tail of the COR header itself: the `CodeManagerTable`,
    /// `VTableFixups` and `ExportAddressTableJumps` directories at offsets 40..64 are 24
    /// bytes that are zero in every image Roslyn emits and that neither PawPrint nor
    /// `MetadataReader` reads. That is the only 16 contiguous bytes at a *computable* RVA
    /// which are known to be both inside a section's `VirtualSize` and unused.
    let private withReadyToRunHeader (flags : uint32, declaredSize : int) (image : byte[]) : byte[] =
        use peImage = new MemoryStream (image)
        use peReader = new PEReader (peImage)
        let headers = peReader.PEHeaders
        let patched = Array.copy image

        // The unused directory trio, as a file offset and as an RVA.
        let headerOffset = headers.CorHeaderStartOffset + 40

        let headerRva =
            headers.SectionHeaders
            |> Seq.tryPick (fun section ->
                if
                    headerOffset >= section.PointerToRawData
                    && headerOffset < section.PointerToRawData + section.SizeOfRawData
                then
                    Some (section.VirtualAddress + (headerOffset - section.PointerToRawData))
                else
                    None
            )
            |> Option.defaultWith (fun () -> failwith "COR header does not lie within any section's raw data")

        let write (offset : int) (bytes : byte[]) = bytes.CopyTo (patched, offset)

        // READYTORUN_HEADER: Signature ("RTR\0"), MajorVersion, MinorVersion, then the core
        // header's Flags and NumberOfSections.
        write headerOffset (System.BitConverter.GetBytes 0x00525452u)
        write (headerOffset + 4) (System.BitConverter.GetBytes 16us)
        write (headerOffset + 6) (System.BitConverter.GetBytes 0us)
        write (headerOffset + 8) (System.BitConverter.GetBytes flags)
        write (headerOffset + 12) (System.BitConverter.GetBytes 0)

        // IMAGE_COR20_HEADER.ManagedNativeHeader, the last of its data directories.
        write (headers.CorHeaderStartOffset + 64) (System.BitConverter.GetBytes headerRva)
        write (headers.CorHeaderStartOffset + 68) (System.BitConverter.GetBytes declaredSize)

        patched

    /// An image whose `PublicKey` blob is non-empty but whose `Flags` column leaves the
    /// `afPublicKey` bit clear. No compiler emits that — Roslyn sets the bit whenever it
    /// signs — but the format permits it, and `GetAssemblyProps` normalises it away, so it
    /// is the only way to observe the synthesis. Hand-built for that reason.
    let private inconsistentPublicKeyAssemblyName =
        "WoofWare.PawPrint.InconsistentPublicKeyTestLibrary"

    /// `GetAssemblyProps` only looks at the blob's *length*, but the blob still has to be a
    /// well-formed key: registering the assembly canonicalises it by `AssemblyName.FullName`,
    /// which computes a public key token and throws on anything it cannot parse. So the
    /// caller passes a real key — corelib's — rather than arbitrary bytes.
    let private inconsistentPublicKeyImage (publicKey : byte array) : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString (inconsistentPublicKeyAssemblyName + ".dll"),
            metadata.GetOrAddGuid (System.Guid "6b1a9f2c-5d3e-4a7b-8c9d-0e1f2a3b4c5d"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString inconsistentPublicKeyAssemblyName,
            System.Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddBlob publicKey,
            // The point of the fixture: a non-empty key blob with the flag left clear.
            Unchecked.defaultof<System.Reflection.AssemblyFlags>,
            System.Reflection.AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// Reads `image` as a `DumpedAssembly` and registers it in `state`, so a QCall keyed by
    /// its full name resolves. Mirrors what the interpreter's own assembly loading does.
    let private withLoadedAssembly
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (image : byte[])
        (state : IlMachineState)
        : DumpedAssembly * IlMachineState
        =
        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage
        assembly, state.WithLoadedAssembly assembly

    let private prepareGuest
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (image : byte[])
        : Program.PreparedProgram
        =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "SimpleNameTestGuest.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected guest to be ready before Main, but got %O{outcome}"

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () ->
            failwith $"type %s{namespaceName}.%s{typeName} not found in %s{assembly.Name.Name}"
        )

    /// The two types in corelib that declare this family's QCall entry points.
    let private runtimeAssembly = ("System.Reflection", "RuntimeAssembly")

    let private moduleHandle = ("System", "ModuleHandle")

    /// Locates the method on `declaringNamespace.declaringTypeName` carrying the given QCall
    /// entry point and concretizes it, so the handler sees the same `ExecutingMethod`
    /// signature the interpreter would have handed it. Most of this family declares its
    /// entry points on `System.Reflection.RuntimeAssembly`, but the two `ModuleHandle_*` ones
    /// `Assembly.GetName()` reaches are on `System.ModuleHandle` instead.
    let private qCallMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringNamespace : string, declaringTypeName : string)
        (entryPoint : string)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib declaringNamespace declaringTypeName

        let rawMethod =
            declaringType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith $"QCall entry point %s{entryPoint} not found on %s{declaringTypeName}"
                | methods ->
                    failwith
                        $"QCall entry point %s{entryPoint} was ambiguous on %s{declaringTypeName}: %d{methods.Length} matches"

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

        state, declaringType, method

    let private concreteValueTypeZero
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ConcreteTypeHandle * CliType * IlMachineState
        =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle
        handle, zero, state

    /// `struct QCallAssembly { void* _ptr; IntPtr _assembly; }`, with `_assembly` carrying
    /// the tag that PawPrint uses in place of CoreCLR's native `Assembly*`.
    let private qCallAssemblyValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyFullName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallAssemblyType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallAssembly"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state qCallAssemblyType

        match zero with
        | CliType.ValueType vt ->
            let assemblyField =
                IlMachineState.requiredOwnInstanceFieldId state handle "_assembly"

            CliValueType.WithFieldSetById
                assemblyField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallAssembly zero value was not a value type: %O{other}"

    /// `struct QCallModule { void* _ptr; IntPtr _module; }`. PawPrint models one module per
    /// assembly, so `_module` carries the assembly's full name exactly as `_assembly` does
    /// above — the two structs differ only in the field name and the tag it holds.
    let private qCallModuleValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyFullName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallModuleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallModule"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state qCallModuleType

        match zero with
        | CliType.ValueType vt ->
            let moduleField = IlMachineState.requiredOwnInstanceFieldId state handle "_module"

            CliValueType.WithFieldSetById
                moduleField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallModule zero value was not a value type: %O{other}"

    /// Mirrors the `new StringHandleOnStack(ref name)` the C# wrapper builds over a local
    /// preinitialised to null; the object[1] cell stands in for that stack slot.
    /// `StringHandleOnStack` and `ObjectHandleOnStack` are both a lone `void* _ptr` wrapping a
    /// byref to the caller's local; only the name differs. The object[1] cell stands in for
    /// that stack slot, and starts null exactly as the C# wrappers' locals do.
    let private handleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handleTypeName : string)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let handleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" handleTypeName

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state handleType

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            let value =
                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType

            value, target, state
        | other -> failwith $"%s{handleTypeName} zero value was not a value type: %O{other}"

    let private stringHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        handleOnStackValue loggerFactory baseClassTypes "StringHandleOnStack" state

    let private objectHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        handleOnStackValue loggerFactory baseClassTypes "ObjectHandleOnStack" state

    /// Allocates an `int[1]` and returns a managed pointer at element 0, standing in for
    /// the caller's `out int` local. Seeded with a value no metadata version column can
    /// hold, so a handler that never wrote is distinguishable from one that wrote 0.
    let private int32OutSlot
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 unwrittenSentinel))
                1
                state

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let private readInt32Out
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int
        =
        match
            IlMachineState.readManagedByref baseClassTypes state ptr
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.Numeric (CliNumericType.Int32 value) -> value
        | other -> failwith $"expected Int32 out value, got %O{other}"

    /// Runs `entryPoint` with the given native arguments against the entry thread, and
    /// returns the state the handler produced. Fails if the handler declines the call or
    /// suspends rather than completing.
    let private invokeQCall
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (declaringType : string * string)
        (entryPoint : string)
        (arguments : CliType list)
        (state : IlMachineState)
        : IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            qCallMethod loggerFactory baseClassTypes declaringType entryPoint state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = method
                Arguments = ImmutableArray.CreateRange arguments
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = declaringTypeInfo
            }

        // Deliberately through `NativeQCall.tryExecute` rather than straight at the owning
        // module's `tryExecuteQCall`. That is the path the interpreter takes, and it derives
        // the entry point from the method's own import metadata, so these tests also fail if
        // a handler exists but was never registered in the dispatch table — which is
        // otherwise an entirely silent mistake.
        match NativeQCall.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some result -> failwith $"unexpected %s{entryPoint} execution result: %O{result}"
        | None -> failwith $"%s{entryPoint} QCall did not match, or is not registered in NativeQCall"

    /// Runs `AssemblyNative_GetFlags` for `assemblyFullName` and pops its return value.
    /// Unlike the rest of this family the answer comes back on the eval stack rather than
    /// through an out-parameter, so there is nothing to preinitialise: a handler that pushed
    /// nothing fails in `popEvalStack` rather than reading back as a sentinel.
    let private invokeGetFlags
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * int
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let state =
            invokeQCall loggerFactory prepared runtimeAssembly "AssemblyNative_GetFlags" [ qCallAssembly ] state

        let returned, state = IlMachineState.popEvalStack prepared.EntryThread state

        // Deliberately strict about the source as well as the width: these flags are read
        // straight out of metadata, so anything but a plain value here (a truncated byref,
        // say) would mean the handler pushed something it had no business pushing.
        match returned with
        | EvalStackValue.Int32 (Int32Source.Verbatim value) -> state, value
        | other -> failwith $"expected a verbatim Int32 return from AssemblyNative_GetFlags, got %O{other}"

    /// Runs `AssemblyNative_GetHashAlgorithm` for `assemblyFullName` and pops its return
    /// value. Same shape as `invokeGetFlags`: the answer is a return value, so a handler
    /// that pushed nothing fails in `popEvalStack` rather than reading back as a sentinel.
    let private invokeGetHashAlgorithm
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * int
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let state =
            invokeQCall loggerFactory prepared runtimeAssembly "AssemblyNative_GetHashAlgorithm" [ qCallAssembly ] state

        let returned, state = IlMachineState.popEvalStack prepared.EntryThread state

        match returned with
        | EvalStackValue.Int32 (Int32Source.Verbatim value) -> state, value
        | other -> failwith $"expected a verbatim Int32 return from AssemblyNative_GetHashAlgorithm, got %O{other}"

    /// Runs `ModuleHandle_GetPEKind` for the manifest module of `assemblyFullName` and reads
    /// back the two `out int` slots. Unlike everything else in this family the answer comes
    /// through two pointers at once, so both slots are seeded and both are read: a handler
    /// that wrote the same value twice, or wrote them in the wrong order, has to be caught
    /// by the values rather than by the shape.
    let private invokeGetPEKind
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * int * int
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallModule, state =
            qCallModuleValue loggerFactory baseClassTypes assemblyFullName state

        let peKindSlot, state = int32OutSlot baseClassTypes state
        let machineSlot, state = int32OutSlot baseClassTypes state

        let state =
            invokeQCall
                loggerFactory
                prepared
                moduleHandle
                "ModuleHandle_GetPEKind"
                [
                    qCallModule
                    CliType.RuntimePointer (CliRuntimePointer.Managed peKindSlot)
                    CliType.RuntimePointer (CliRuntimePointer.Managed machineSlot)
                ]
                state

        state, readInt32Out baseClassTypes state peKindSlot, readInt32Out baseClassTypes state machineSlot

    /// Runs `ModuleHandle_GetMDStreamVersion` for the manifest module of `assemblyFullName`
    /// and pops its return value. Unlike the rest of this family it is keyed by a
    /// `QCallModule` and declared on `System.ModuleHandle` rather than `RuntimeAssembly`.
    let private invokeGetMDStreamVersion
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * int
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallModule, state =
            qCallModuleValue loggerFactory baseClassTypes assemblyFullName state

        let state =
            invokeQCall loggerFactory prepared moduleHandle "ModuleHandle_GetMDStreamVersion" [ qCallModule ] state

        let returned, state = IlMachineState.popEvalStack prepared.EntryThread state

        match returned with
        | EvalStackValue.Int32 (Int32Source.Verbatim value) -> state, value
        | other -> failwith $"expected a verbatim Int32 return from ModuleHandle_GetMDStreamVersion, got %O{other}"

    /// Runs the QCall for `assemblyFullName` and returns the heap address the handler wrote
    /// into the `StringHandleOnStack` (None if it left the slot at its preinitialised null).
    /// Shared by every `(QCallAssembly, StringHandleOnStack) -> void` entry point.
    let private invokeStringQCall
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (entryPoint : string)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let stringHandle, target, state =
            stringHandleOnStackValue loggerFactory baseClassTypes state

        let state =
            invokeQCall loggerFactory prepared runtimeAssembly entryPoint [ qCallAssembly ; stringHandle ] state

        let written =
            match IlMachineState.readManagedByref baseClassTypes state target with
            | CliType.ObjectRef maybeAddr -> maybeAddr
            | other -> failwith $"expected StringHandleOnStack target to contain an object ref, got %O{other}"

        state, written

    let private invokeGetSimpleName
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option
        =
        invokeStringQCall loggerFactory prepared "AssemblyNative_GetSimpleName" state assemblyFullName

    /// Runs `AssemblyNative_GetPublicKey` for `assemblyFullName` and returns the heap address
    /// the handler wrote into the `ObjectHandleOnStack` (None if it left the slot at null).
    let private invokeGetPublicKey
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let objectHandle, target, state =
            objectHandleOnStackValue loggerFactory baseClassTypes state

        let state =
            invokeQCall
                loggerFactory
                prepared
                runtimeAssembly
                "AssemblyNative_GetPublicKey"
                [ qCallAssembly ; objectHandle ]
                state

        let written =
            match IlMachineState.readManagedByref baseClassTypes state target with
            | CliType.ObjectRef maybeAddr -> maybeAddr
            | other -> failwith $"expected ObjectHandleOnStack target to contain an object ref, got %O{other}"

        state, written

    /// Runs `AssemblyNative_GetCodeBase`, which is the only one in this family that answers
    /// with *both* a written string and a return value, so both come back here: the address
    /// it wrote into the `StringHandleOnStack` (None if it left the slot at its
    /// preinitialised null) and the marshalled `BOOL`.
    let private invokeGetCodeBase
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option * int
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        let stringHandle, target, state =
            stringHandleOnStackValue loggerFactory baseClassTypes state

        let state =
            invokeQCall
                loggerFactory
                prepared
                runtimeAssembly
                "AssemblyNative_GetCodeBase"
                [ qCallAssembly ; stringHandle ]
                state

        let returned, state = IlMachineState.popEvalStack prepared.EntryThread state

        let returned =
            match returned with
            | EvalStackValue.Int32 (Int32Source.Verbatim value) -> value
            | other -> failwith $"expected a verbatim Int32 return from AssemblyNative_GetCodeBase, got %O{other}"

        let written =
            match IlMachineState.readManagedByref baseClassTypes state target with
            | CliType.ObjectRef maybeAddr -> maybeAddr
            | other -> failwith $"expected StringHandleOnStack target to contain an object ref, got %O{other}"

        state, written, returned

    let private invokeGetLocale
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * ManagedHeapAddress option
        =
        invokeStringQCall loggerFactory prepared "AssemblyNative_GetLocale" state assemblyFullName

    /// Runs `AssemblyNative_GetVersion` for `assemblyFullName` and reads back the four
    /// `out int` slots, in the declared parameter order (major, minor, build, revision).
    let private invokeGetVersion
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * (int * int * int * int)
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallAssembly, state =
            qCallAssemblyValue loggerFactory baseClassTypes assemblyFullName state

        // Four separate slots, so a handler that wrote one pointer four times, or wrote
        // through the wrong one, cannot look correct.
        let majorPtr, state = int32OutSlot baseClassTypes state
        let minorPtr, state = int32OutSlot baseClassTypes state
        let buildPtr, state = int32OutSlot baseClassTypes state
        let revisionPtr, state = int32OutSlot baseClassTypes state

        let pointerArgument (ptr : ManagedPointerSource) : CliType =
            CliType.RuntimePointer (CliRuntimePointer.Managed ptr)

        let state =
            invokeQCall
                loggerFactory
                prepared
                runtimeAssembly
                "AssemblyNative_GetVersion"
                [
                    qCallAssembly
                    pointerArgument majorPtr
                    pointerArgument minorPtr
                    pointerArgument buildPtr
                    pointerArgument revisionPtr
                ]
                state

        let read = readInt32Out baseClassTypes state

        state, (read majorPtr, read minorPtr, read buildPtr, read revisionPtr)

    /// Asserts that `addr` is a genuine `byte[]` — element type included, so an array of the
    /// right values but the wrong width cannot pass — carrying exactly `expected`.
    let private assertIsByteArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (expected : byte array)
        : unit
        =
        let array =
            match HeapObserver.tryGetArray addr state.ManagedHeap with
            | Some array -> array
            | None -> failwith $"expected a live byte array at %O{addr}"

        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        array.Shape.ConcreteType
        |> shouldEqual (ConcreteTypeHandle.OneDimArrayZero byteHandle)

        let actual =
            array.Elements
            |> Seq.map (fun element ->
                match CliType.unwrapPrimitiveLikeDeep element with
                | CliType.Numeric (CliNumericType.UInt8 b) -> b
                | other -> failwith $"expected byte element, got %O{other}"
            )
            |> Seq.toArray

        actual |> shouldEqual expected

    /// Runs `AssemblyNative_GetFullName` for `assemblyFullName` and reads back the string it
    /// wrote. Unlike the other `StringHandleOnStack` entry points in this family the answer is
    /// compared as a whole string rather than asserted piecewise, so it is read out rather
    /// than checked in place.
    let private invokeGetFullName
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (assemblyFullName : string)
        : IlMachineState * string
        =
        let state, written =
            invokeStringQCall loggerFactory prepared "AssemblyNative_GetFullName" state assemblyFullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        let contents =
            ManagedHeap.getStringContents addr state.ManagedHeap
            |> Option.defaultWith (fun () -> failwith "handler wrote something that is not a string")

        state, contents

    /// Asserts that `addr` is a genuine `System.String` heap object carrying `expected`,
    /// rather than merely a side-table entry.
    let private assertIsString
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (expected : string)
        : unit
        =
        ManagedHeap.getStringContents addr state.ManagedHeap
        |> shouldEqual (Some expected)

        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let stringHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.String

        heapObj.ConcreteType |> shouldEqual stringHandle

        let lengthField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "_stringLength"

        AllocatedNonArrayObject.DereferenceFieldById lengthField heapObj
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 expected.Length))

    [<Test>]
    let ``GetSimpleName returns the Assembly metadata row's name`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        // Sanity: the oracle is reading the name we asked for, so the assertions below
        // are not vacuously comparing two copies of the same mistake.
        metadataAssemblyName image |> shouldEqual guestAssemblyName

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        // The handle the QCall is keyed by is the *full* name, which carries version,
        // culture and public key token on top of the simple name. The point of the test
        // is that the handler answers with the metadata field rather than a prefix of
        // this string.
        guest.Name.FullName |> shouldNotEqual guestAssemblyName

        guest.Name.FullName.StartsWith (guestAssemblyName + ", ", System.StringComparison.Ordinal)
        |> shouldEqual true

        let state, written =
            invokeGetSimpleName loggerFactory prepared prepared.State guest.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr (metadataAssemblyName image)

    [<Test>]
    let ``GetSimpleName answers for corelib too`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes

        let state, written =
            invokeGetSimpleName loggerFactory prepared prepared.State baseClassTypes.Corelib.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr "System.Private.CoreLib"

    [<Test>]
    let ``GetSimpleName allocates a fresh string per call`` () : unit =
        // CoreCLR's `StringHandleOnStack::Set(LPCUTF8)` goes through
        // `StringObject::NewString`, which interns nothing above zero length, so
        // `ReferenceEquals` across two calls is false there. Guest code that cached the
        // result by reference would be relying on behaviour the real runtime does not
        // provide, so PawPrint must not accidentally supply it.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, first =
            invokeGetSimpleName loggerFactory prepared prepared.State guest.Name.FullName

        let _state, second =
            invokeGetSimpleName loggerFactory prepared state guest.Name.FullName

        first |> shouldNotEqual None
        second |> shouldNotEqual None
        first |> shouldNotEqual second

    [<Test>]
    let ``GetSimpleName on an unloaded assembly fails loudly`` () : unit =
        // The handle decodes to an assembly identity we have never loaded, which means a
        // caller invented one. Answering anything at all would be a guess.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetSimpleName
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * ManagedHeapAddress option>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetVersion writes the four Assembly metadata row columns`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        // Sanity: the compiler honoured the [assembly: AssemblyVersion] we asked for, so
        // the four expected components below really are four distinct non-zero numbers
        // and the ordering assertions can catch a transposition.
        let _, metadataVersion = metadataAssemblyDefinition image
        metadataVersion |> shouldEqual guestAssemblyVersion

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let _state, (major, minor, build, revision) =
            invokeGetVersion loggerFactory prepared prepared.State guest.Name.FullName

        (major, minor, build, revision)
        |> shouldEqual (metadataVersion.Major, metadataVersion.Minor, metadataVersion.Build, metadataVersion.Revision)

    [<Test>]
    let ``GetVersion answers per assembly rather than with a constant`` () : unit =
        // Corelib carries the shared framework's own version, which is not the guest's, so
        // a handler ignoring its QCallAssembly argument cannot satisfy both. Asserting
        // corelib's exact version would just restate the value PawPrint parsed from the
        // same metadata row, so assert the structural facts instead: a well-formed
        // four-component version, in the range the metadata columns can hold, that is not
        // the guest's.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, guestVersion =
            invokeGetVersion loggerFactory prepared prepared.State guest.Name.FullName

        let _state, corelibVersion =
            invokeGetVersion loggerFactory prepared state prepared.BaseClassTypes.Corelib.Name.FullName

        corelibVersion |> shouldNotEqual guestVersion

        let corelibMajor, corelibMinor, corelibBuild, corelibRevision = corelibVersion

        for component_ in [ corelibMajor ; corelibMinor ; corelibBuild ; corelibRevision ] do
            // Never the sentinel (so every slot was written), and inside the range a
            // USHORT metadata column can hold.
            component_ |> shouldBeGreaterThan -1
            component_ |> shouldBeSmallerThan (int System.UInt16.MaxValue + 1)

        // A shared framework's corelib is never version 0.0.0.0; if it were, the
        // "differs from the guest" assertion above would be passing for the wrong reason.
        corelibMajor |> shouldBeGreaterThan 0

    [<Test>]
    let ``GetVersion on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetVersion
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * (int * int * int * int)>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``CultureName is the raw Culture column, not the CultureInfo-normalised name`` () : unit =
        // The distinction the handler depends on. `AssemblyName` runs the column through
        // `CultureInfo`, so it cannot be the source for a QCall that must hand the guest
        // `md.szLocale` verbatim.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = culturedLibraryImage ()

        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        assembly.CultureName |> shouldEqual culturedColumn

        // Sanity: the two really do disagree, so the assertion above cannot pass merely
        // because the compiler already canonicalised the column.
        culturedColumn |> shouldNotEqual culturedNormalised
        assembly.Name.CultureName |> shouldEqual culturedNormalised

    [<Test>]
    let ``CultureName is empty for a culture-neutral assembly`` () : unit =
        // A nil `Culture` handle resolves to offset 0 of the `#Strings` heap, which is the
        // empty string — the same thing CoreCLR's `getString` returns, and not null.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        assembly.CultureName |> shouldEqual ""

    [<Test>]
    let ``GetLocale writes the Culture column verbatim`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let cultured, state =
            withLoadedAssembly loggerFactory (culturedLibraryImage ()) prepared.State

        let state, written =
            invokeGetLocale loggerFactory prepared state cultured.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        // Not `culturedNormalised`: a handler sourcing this from `AssemblyName` would write
        // that instead, and the guest's `CultureInfo.GetCultureInfo` would then be handed a
        // string CoreCLR never produced.
        assertIsString prepared.BaseClassTypes state addr culturedColumn

    [<Test>]
    let ``GetLocale writes the canonical empty string for a culture-neutral assembly`` () : unit =
        // CoreCLR reaches `retString.Set("")` here rather than leaving the handle untouched:
        // the pointer it guards on is non-null because a nil `Culture` index resolves into
        // the `#Strings` heap. That matters to the guest, which branches on `locale == null`
        // before calling `CultureInfo.GetCultureInfo(locale)` — the empty string takes the
        // second path, and `GetCultureInfo("")` is the invariant culture.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, guestLocale =
            invokeGetLocale loggerFactory prepared prepared.State guest.Name.FullName

        let guestAddr =
            guestLocale
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state guestAddr ""

        // A framework assembly is culture-neutral too.
        let state, corelibLocale =
            invokeGetLocale loggerFactory prepared state baseClassTypes.Corelib.Name.FullName

        let corelibAddr =
            corelibLocale
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state corelibAddr ""

        // `StringObject::NewString` hands back the shared empty-string instance for a
        // zero-length string, so these are reference-identical on CoreCLR and must be here.
        let canonicalEmpty, _state =
            IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

        guestAddr |> shouldEqual canonicalEmpty
        corelibAddr |> shouldEqual canonicalEmpty

    [<Test>]
    let ``GetLocale on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetLocale
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * ManagedHeapAddress option>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``PublicKey is the raw blob column, not the AssemblyName view`` () : unit =
        // The distinction the handler depends on, and the mirror of the `CultureName` one:
        // `AssemblyName` reports null for an assembly with no key, where the column is a
        // zero-length blob and CoreCLR hands the guest an empty array.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        assembly.PublicKey.IsDefault |> shouldEqual false
        assembly.PublicKey.Length |> shouldEqual 0

        // Sanity: the two really do disagree, so the assertion above distinguishes them.
        assembly.Name.GetPublicKey () |> isNull |> shouldEqual true

    [<Test>]
    let ``GetPublicKey writes an empty array, not null, for an assembly with no key`` () : unit =
        // `ObjectHandleOnStack::SetByteArray` allocates and writes unconditionally — there is
        // no null guard here at all, unlike `GetLocale` — so the caller's preinitialised
        // `byte[]? publicKey = null` is always overwritten, and an unsigned assembly's key
        // reads back as `byte[0]` rather than null.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, written =
            invokeGetPublicKey loggerFactory prepared prepared.State guest.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the ObjectHandleOnStack at null")

        assertIsByteArray prepared.BaseClassTypes state addr [||]

    [<Test>]
    let ``GetPublicKey writes the full key of a strong-named assembly`` () : unit =
        // Corelib is strong-named, so this is the non-empty half. The oracle is
        // `AssemblyName.GetPublicKey()`: an independent parse of the same row, which agrees
        // with the raw column whenever a key is actually present (it is only the absent case
        // where it reports null instead of empty).
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let corelib = prepared.BaseClassTypes.Corelib

        let expected = corelib.Name.GetPublicKey ()

        // Sanity: corelib really is signed, so this test is not silently the empty case again.
        expected |> isNull |> shouldEqual false
        expected.Length |> shouldBeGreaterThan 0

        let state, written =
            invokeGetPublicKey loggerFactory prepared prepared.State corelib.Name.FullName

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the ObjectHandleOnStack at null")

        assertIsByteArray prepared.BaseClassTypes state addr expected

    [<Test>]
    let ``GetPublicKey on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetPublicKey
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * ManagedHeapAddress option>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``Flags is the raw column, not the masked AssemblyName view`` () : unit =
        // Third instance of the same trap, and the only one where `AssemblyName` agrees with
        // the column on everything a compiler normally emits — the disagreement is confined
        // to bits it exposes as separate properties. So the test has to reach for a flag in
        // the masked range rather than a typical one.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (maskedFlagsLibraryImage ())
        let assembly = Assembly.read loggerFactory None peImage

        int assembly.Flags |> shouldEqual maskedFlagsColumn

        // Sanity: the two really do disagree here, so the assertion above distinguishes them.
        int assembly.Name.Flags |> shouldEqual 0

    [<Test>]
    let ``GetFlags returns bits the masked AssemblyName view would drop`` () : unit =
        // The end-to-end version of the test above: a handler sourcing this from
        // `AssemblyName.Flags` would return 0 here, and the guest's `RawFlags` — which is
        // where the caller puts this value precisely because it is unmasked — would lose
        // the assembly's content type.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let masked, state =
            withLoadedAssembly loggerFactory (maskedFlagsLibraryImage ()) prepared.State

        let _state, flags = invokeGetFlags loggerFactory prepared state masked.Name.FullName

        flags |> shouldEqual maskedFlagsColumn

    [<Test>]
    let ``GetFlags returns the manifest row's flags per assembly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        // Oracle for the guest: the column read independently from the same image.
        let expectedGuestFlags =
            use peImage = new MemoryStream (image)
            use peReader = new System.Reflection.PortableExecutable.PEReader (peImage)
            let metadata = peReader.GetMetadataReader ()
            int (metadata.GetAssemblyDefinition().Flags)

        // Roslyn emits no assembly flags for an ordinary unsigned library, so this is the
        // "nothing set" end of the range. Stated as a sanity check rather than assumed.
        expectedGuestFlags |> shouldEqual 0

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, guestFlags =
            invokeGetFlags loggerFactory prepared prepared.State guest.Name.FullName

        guestFlags |> shouldEqual expectedGuestFlags

        // Corelib is strong-named — `GetPublicKey writes the full key of a strong-named
        // assembly` establishes that independently — so ECMA-335 II.23.1.2's PublicKey bit
        // must be set in its column. That also makes this a second assembly with a
        // different answer, so a handler ignoring its argument cannot satisfy both.
        let _state, corelibFlags =
            invokeGetFlags loggerFactory prepared state prepared.BaseClassTypes.Corelib.Name.FullName

        let publicKeyBit = 0x1
        corelibFlags &&& publicKeyBit |> shouldEqual publicKeyBit
        corelibFlags |> shouldNotEqual guestFlags

    [<Test>]
    let ``GetFlags on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetFlags
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * int>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetFlags synthesises the PublicKey bit from a non-empty key blob`` () : unit =
        // `GetAssemblyProps` — the metadata-import call CoreCLR reads this column through —
        // ORs in `afPublicKey` whenever the `PublicKey` blob is non-empty, whatever the
        // column says. Returning the column verbatim would diverge for such an image.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        // Corelib's own key: a real, parseable blob, so registering the fixture below does
        // not trip over public-key-token computation.
        let realPublicKey = prepared.BaseClassTypes.Corelib.PublicKey.AsSpan().ToArray ()

        let inconsistent, state =
            withLoadedAssembly loggerFactory (inconsistentPublicKeyImage realPublicKey) prepared.State

        // Sanity: the fixture really is the awkward shape — key present, column bit clear —
        // so the assertion below cannot pass just by echoing the column.
        inconsistent.PublicKey.IsEmpty |> shouldEqual false
        int inconsistent.Flags |> shouldEqual 0

        let _state, flags =
            invokeGetFlags loggerFactory prepared state inconsistent.Name.FullName

        let afPublicKey = 0x1
        flags |> shouldEqual afPublicKey

    [<Test>]
    let ``GetHashAlgorithm returns the manifest row's hash algorithm per assembly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        // Oracle for the guest: the column read independently from the same image.
        let expectedGuestHashAlgorithm =
            use peImage = new MemoryStream (image)
            use peReader = new System.Reflection.PortableExecutable.PEReader (peImage)
            let metadata = peReader.GetMetadataReader ()
            int (metadata.GetAssemblyDefinition().HashAlgorithm)

        // Stated as a sanity check rather than assumed: Roslyn's default is SHA1, which is
        // also what makes `hashAlgorithmLibraryImage` necessary below.
        let sha1 = 0x8004
        expectedGuestHashAlgorithm |> shouldEqual sha1

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, guestHashAlgorithm =
            invokeGetHashAlgorithm loggerFactory prepared prepared.State guest.Name.FullName

        guestHashAlgorithm |> shouldEqual expectedGuestHashAlgorithm

        // Corelib is SHA1 as well, so it is not a second answer — assert that rather than
        // leaving it implied, since it is the reason the fixture below exists at all.
        let state, corelibHashAlgorithm =
            invokeGetHashAlgorithm loggerFactory prepared state prepared.BaseClassTypes.Corelib.Name.FullName

        corelibHashAlgorithm |> shouldEqual sha1

        // The assembly that does give a different answer, so a handler ignoring its argument
        // — or returning a constant SHA1, which would satisfy both assertions above — cannot
        // pass. Its column also names no `AssemblyHashAlgorithm` case, so this pins down that
        // the value travels through uninterpreted.
        let custom, state =
            withLoadedAssembly loggerFactory (hashAlgorithmLibraryImage ()) state

        let _state, customHashAlgorithm =
            invokeGetHashAlgorithm loggerFactory prepared state custom.Name.FullName

        customHashAlgorithm |> shouldEqual hashAlgorithmColumn
        customHashAlgorithm |> shouldNotEqual guestHashAlgorithm

    [<Test>]
    let ``GetHashAlgorithm on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetHashAlgorithm
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * int>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetMDStreamVersion packs the table stream's schema version`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        // `MD_STREAM_VER_2` in metadata.h: major 2 in the senior half, minor 0 in the junior.
        // This is what every modern image says, which is precisely the problem the fixture
        // below solves.
        let mdStreamVer2 = 0x20000

        let state, guestVersion =
            invokeGetMDStreamVersion loggerFactory prepared prepared.State guest.Name.FullName

        guestVersion |> shouldEqual mdStreamVer2

        let state, corelibVersion =
            invokeGetMDStreamVersion loggerFactory prepared state prepared.BaseClassTypes.Corelib.Name.FullName

        corelibVersion |> shouldEqual mdStreamVer2

        // The only assemblies that give a different answer, and so the ones that distinguish
        // reading the header from returning `MD_STREAM_VER_2`. It matters beyond this
        // handler: `Assembly.GetName()` reads the PE kind only when this exceeds `0x10000`,
        // so a 1.0 image takes the other branch, and a hardcoded answer would silently take
        // the wrong one rather than fail.
        //
        // Separately named libraries rather than patched copies of the guest, because
        // assemblies are registered by full name and same-named copies would displace each
        // other (and the guest).
        let oneZero, state =
            withLoadedAssembly
                loggerFactory
                (streamVersionLibraryImage "OneZero" |> withTableStreamVersion (1uy, 0uy))
                state

        let state, oneZeroVersion =
            invokeGetMDStreamVersion loggerFactory prepared state oneZero.Name.FullName

        let mdStreamVer1X = 0x10000
        oneZeroVersion |> shouldEqual mdStreamVer1X

        // A non-zero minor, so the two halves cannot be swapped and the minor cannot be
        // dropped: this is the only case where `minor ||| (major <<< 16)` differs both from
        // `major <<< 16` and from `major ||| (minor <<< 16)`.
        let threeSeven, state =
            withLoadedAssembly
                loggerFactory
                (streamVersionLibraryImage "ThreeSeven" |> withTableStreamVersion (3uy, 7uy))
                state

        let _state, threeSevenVersion =
            invokeGetMDStreamVersion loggerFactory prepared state threeSeven.Name.FullName

        threeSevenVersion |> shouldEqual 0x30007

    [<Test>]
    let ``GetMDStreamVersion on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetMDStreamVersion
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * int>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetCodeBase reports no code base, and still writes the string`` () : unit =
        // PawPrint takes `PEAssembly::GetCodeBase`'s `else` branch for every assembly — the
        // one for an image in a bundle or external data, with no path to turn into a
        // `file://` URL. See docs/divergences.md; it is the same no-file-backing position
        // `AssemblyNative_GetLocation` already takes.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, written, returned =
            invokeGetCodeBase loggerFactory prepared prepared.State guest.Name.FullName

        // FALSE: the BCL marshals this to `bool` and returns null for the code base when it
        // is false. Any non-zero value would instead hand the guest the empty string as if
        // it were a real code base.
        returned |> shouldEqual 0

        // CoreCLR's `retString.Set(codebase)` sits *outside* its `if`, so the string is
        // written on the false branch too. A handler that skipped the write would leave the
        // caller's local at null — which happens to reach the same managed answer here, but
        // is not what the primitive does, and would be wrong the moment the bool is true.
        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr ""

        // The shared empty-string instance, as `StringObject::NewString` returns for a
        // zero-length string.
        let canonicalEmpty, _state =
            IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

        addr |> shouldEqual canonicalEmpty

    [<Test>]
    let ``GetCodeBase reports no code base for a framework assembly too`` () : unit =
        // Corelib resolves from the host's runtime directories, so this is the case where
        // reporting a real path would leak the developer's machine into a replay.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes

        let state, written, returned =
            invokeGetCodeBase loggerFactory prepared prepared.State baseClassTypes.Corelib.Name.FullName

        returned |> shouldEqual 0

        let addr =
            written
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString baseClassTypes state addr ""

    [<Test>]
    let ``GetCodeBase on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetCodeBase
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * ManagedHeapAddress option * int>
            )

        exn.Message |> shouldContainText "is not loaded"

    /// `CorPEKind` (corhdr.h), the bitfield `GetPEKind` writes through its first pointer.
    let private peILonly = 0x1
    let private pe32BitRequired = 0x2
    let private pe32Plus = 0x4
    let private pe32BitPreferred = 0x10

    let private headersOf
        (machine : Machine)
        (isPE32Plus : bool)
        (corFlags : CorFlags)
        (readyToRun : ReadyToRunHeader option)
        : PEImageHeaders
        =
        {
            Machine = machine
            IsPE32Plus = isPE32Plus
            CorFlags = corFlags
            ReadyToRunHeader = readyToRun
        }

    /// `IMAGE_FILE_MACHINE_NATIVE_NI` as a linux-x64 CoreCLR computes it: AMD64 XORed with
    /// the `__linux__` discriminator `0x7B79` (coreclr/inc/pedecoder.h). This is the machine
    /// crossgen2 stamps into a linux-x64 ReadyToRun image, and deliberately names no
    /// `ImageFileMachine` case at all — which is why an implementation that reported it
    /// verbatim would be so visibly wrong.
    let private linuxX64NativeNiMachine : Machine =
        LanguagePrimitives.EnumOfValue (uint16 Machine.Amd64 ^^^ 0x7B79us)

    /// The shape crossgen2 emits: version 16.0, and the platform-neutral bit alongside
    /// several others, so a handler that compared the whole flags word against a constant
    /// rather than masking the bit would fail.
    let private readyToRunNeutral : ReadyToRunHeader =
        {
            MajorVersion = 16
            MinorVersion = 0
            Flags = 0x4Bu
        }

    [<Test>]
    let ``peKindAndMachine decodes the header combinations CoreCLR distinguishes`` () : unit =
        // Each case is `PEDecoder::GetPEKindAndMachine` (coreclr/inc/pedecoder.inl) applied by
        // hand. Driving the decoder directly rather than through an image is what makes the
        // arms reachable at all: no compiler emits the MC++ shape, and the ReadyToRun arms
        // need a crossgen2 image.
        let decode (headers : PEImageHeaders) : int * int =
            let result = NativeModuleHandle.peKindAndMachine "test" headers
            result.PEKind, result.Machine

        // AnyCPU, which is every image the test harness compiles and every non-ReadyToRun
        // framework assembly.
        decode (headersOf Machine.I386 false CorFlags.ILOnly None)
        |> shouldEqual (peILonly, int Machine.I386)

        // A RID-specific publish: IL-only, but pinned to one architecture, so `pe32Plus`
        // survives and the machine is the real one.
        decode (headersOf Machine.Amd64 true CorFlags.ILOnly None)
        |> shouldEqual (peILonly ||| pe32Plus, int Machine.Amd64)

        // CoreCLR's `HOST_64BIT` compensation: PE32+ *and* I386 *and* IL-only means the
        // Windows shim promoted a PE32 header in memory, so the PE32+ bit is a lie and gets
        // cleared. Only the conjunction does it — the case above keeps its bit.
        decode (headersOf Machine.I386 true CorFlags.ILOnly None)
        |> shouldEqual (peILonly, int Machine.I386)

        // `/platform:x86`. 32BITREQUIRED set and 32BITPREFERRED clear is the two-bit field's
        // "image is x86-specific".
        decode (headersOf Machine.I386 false (CorFlags.ILOnly ||| CorFlags.Requires32Bit) None)
        |> shouldEqual (peILonly ||| pe32BitRequired, int Machine.I386)

        // `/platform:anycpu32bitpreferred`. Both bits set is a *different* value of the same
        // field, not the sum of two flags, so this must not also report `pe32BitRequired`.
        decode (
            headersOf Machine.I386 false (CorFlags.ILOnly ||| CorFlags.Requires32Bit ||| CorFlags.Prefers32Bit) None
        )
        |> shouldEqual (peILonly ||| pe32BitPreferred, int Machine.I386)

        // 32BITPREFERRED without 32BITREQUIRED is the field's fourth value, which corhdr.h
        // calls "illegal, reserved for future use". CoreCLR's macros match neither, so
        // neither bit is reported.
        decode (headersOf Machine.I386 false (CorFlags.ILOnly ||| CorFlags.Prefers32Bit) None)
        |> shouldEqual (peILonly, int Machine.I386)

        // The "MC++ peculiarity": a managed PE32 image that is neither IL-only nor flagged
        // 32-bit would otherwise decode to `peNot` (0), which claims it is not a PE file at
        // all, so CoreCLR substitutes `pe32BitRequired`.
        decode (headersOf Machine.I386 false CorFlags.StrongNameSigned None)
        |> shouldEqual (pe32BitRequired, int Machine.I386)

        // ReadyToRun compiled from platform-neutral IL: report what that IL reported, so the
        // assembly name still looks the way it did before the AOT step. Note how much this
        // discards — PE32+, the AOT machine, and the absence of `ILOnly` all vanish.
        decode (
            headersOf
                linuxX64NativeNiMachine
                true
                (CorFlags.ILLibrary ||| CorFlags.StrongNameSigned)
                (Some readyToRunNeutral)
        )
        |> shouldEqual (peILonly, int Machine.I386)

    [<Test>]
    let ``peKindAndMachine refuses a ReadyToRun image compiled from architecture-specific IL`` () : unit =
        // The one case whose answer depends on which architecture's native runtime is
        // executing: CoreCLR would rewrite an `IMAGE_FILE_MACHINE_NATIVE_NI` machine back to
        // `IMAGE_FILE_MACHINE_NATIVE`, both of which are fixed when that runtime is built.
        // Refusing is confined to exactly this case because the platform-neutral arm above
        // overwrites the machine regardless, so it is the only place the identity shows.
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                NativeModuleHandle.peKindAndMachine
                    "test"
                    (headersOf
                        linuxX64NativeNiMachine
                        true
                        (CorFlags.ILLibrary ||| CorFlags.StrongNameSigned)
                        (Some
                            { readyToRunNeutral with
                                Flags = readyToRunNeutral.Flags &&& ~~~0x1u
                            }))
                |> ignore<PEKindAndMachine>
            )

        exn.Message |> shouldContainText "READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE"

    [<Test>]
    let ``GetPEKind reports the image's own kind and machine`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        // Pin what the guest image actually is, so the expected answer below is anchored to
        // its headers rather than to a guess about what Roslyn emits.
        let guestFacts = imageHeaderFacts image
        guestFacts.Machine |> shouldEqual Machine.I386
        guestFacts.IsPE32Plus |> shouldEqual false
        guestFacts.CorFlags |> shouldEqual CorFlags.ILOnly
        guestFacts.ReadyToRunFlags |> shouldEqual None

        let state, guestKind, guestMachine =
            invokeGetPEKind loggerFactory prepared prepared.State guest.Name.FullName

        guestKind |> shouldEqual peILonly
        guestMachine |> shouldEqual (int Machine.I386)

        // Separately named libraries rather than patched copies of the guest, because
        // assemblies are registered by full name and same-named copies would displace each
        // other (and the guest).
        //
        // Every image the harness compiles is I386, which is also the value the ReadyToRun
        // platform-neutral arm substitutes — so without a differently-machined image, a
        // handler that never read the COFF header at all would pass.
        let amd64, state =
            withLoadedAssembly loggerFactory (peKindLibraryImage "Amd64" |> withMachine Machine.Amd64) state

        let state, amd64Kind, amd64Machine =
            invokeGetPEKind loggerFactory prepared state amd64.Name.FullName

        amd64Kind |> shouldEqual peILonly
        amd64Machine |> shouldEqual (int Machine.Amd64)

        // The COR flags likewise: the harness compiles AnyCPU only, so the 32-bit arms need a
        // patched image to be reachable through a real one at all.
        let required, state =
            withLoadedAssembly
                loggerFactory
                (peKindLibraryImage "Required"
                 |> withCorFlags (CorFlags.ILOnly ||| CorFlags.Requires32Bit))
                state

        let state, requiredKind, requiredMachine =
            invokeGetPEKind loggerFactory prepared state required.Name.FullName

        requiredKind |> shouldEqual (peILonly ||| pe32BitRequired)
        requiredMachine |> shouldEqual (int Machine.I386)

        let preferred, state =
            withLoadedAssembly
                loggerFactory
                (peKindLibraryImage "Preferred"
                 |> withCorFlags (CorFlags.ILOnly ||| CorFlags.Requires32Bit ||| CorFlags.Prefers32Bit))
                state

        let _state, preferredKind, preferredMachine =
            invokeGetPEKind loggerFactory prepared state preferred.Name.FullName

        preferredKind |> shouldEqual (peILonly ||| pe32BitPreferred)
        preferredMachine |> shouldEqual (int Machine.I386)

    [<Test>]
    let ``GetPEKind reports the pre-AOT identity of a ReadyToRun framework assembly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let corelib = prepared.BaseClassTypes.Corelib

        let corelibPath =
            corelib.OriginalPath
            |> Option.defaultWith (fun () ->
                failwith "corelib was not loaded from a file, so its bytes cannot be read independently"
            )

        let corelibFacts = imageHeaderFacts (File.ReadAllBytes corelibPath)

        // Assert the shape before the answer. Every assembly in a shipped shared framework is
        // ReadyToRun, and its raw headers say something quite different from what the guest
        // is entitled to see: no `ILOnly` bit, PE32+, and a machine that is the real
        // architecture XORed with an OS discriminator (`IMAGE_FILE_MACHINE_NATIVE_NI`) and so
        // names no `ImageFileMachine` case at all. If a future framework stopped shipping
        // ReadyToRun images, the answer below would still be right but would no longer be
        // testing the ReadyToRun arm, and this is what says so.
        let readyToRunFlags =
            corelibFacts.ReadyToRunFlags
            |> Option.defaultWith (fun () ->
                failwith
                    $"corelib at %s{corelibPath} has no ReadyToRun header, so this test no longer exercises the ReadyToRun arm"
            )

        readyToRunFlags &&& 0x1u |> shouldEqual 0x1u
        corelibFacts.IsPE32Plus |> shouldEqual true
        corelibFacts.CorFlags &&& CorFlags.ILOnly |> shouldEqual (enum<CorFlags> 0)
        corelibFacts.Machine |> shouldNotEqual Machine.I386

        let _state, corelibKind, corelibMachine =
            invokeGetPEKind loggerFactory prepared prepared.State corelib.Name.FullName

        // What the IL corelib was compiled from said: AnyCPU.
        corelibKind |> shouldEqual peILonly
        corelibMachine |> shouldEqual (int Machine.I386)

    [<Test>]
    let ``GetPEKind on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetPEKind
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * int * int>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetPEKind reads the ReadyToRun header out of a real image`` () : unit =
        // Companion to the framework-assembly test: that one proves the ReadyToRun arm on the
        // only images that actually carry a ReadyToRun header, but it cannot vary them. This
        // one synthesises the header, so the accept/reject decision itself can be varied
        // while everything else about the image stays fixed.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        // Every image the harness compiles is I386, which is also what the platform-neutral
        // arm substitutes — so the base image is re-machined to AMD64 first. That makes
        // "ReadyToRun header honoured" and "ReadyToRun header ignored" give *different*
        // machines, which is the whole point: otherwise both would answer I386 and the
        // fixtures below could not tell them apart.
        let baseImage (suffix : string) : byte[] =
            peKindLibraryImage suffix |> withMachine Machine.Amd64

        let ignored, state =
            withLoadedAssembly loggerFactory (baseImage "NoR2R") prepared.State

        let state, ignoredKind, ignoredMachine =
            invokeGetPEKind loggerFactory prepared state ignored.Name.FullName

        ignoredKind |> shouldEqual peILonly
        ignoredMachine |> shouldEqual (int Machine.Amd64)

        // READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE alongside the other bits crossgen2 sets.
        let neutral, state =
            withLoadedAssembly loggerFactory (baseImage "Neutral" |> withReadyToRunHeader (0x4Bu, 16)) state

        let state, neutralKind, neutralMachine =
            invokeGetPEKind loggerFactory prepared state neutral.Name.FullName

        neutralKind |> shouldEqual peILonly
        neutralMachine |> shouldEqual (int Machine.I386)

        // A `ManagedNativeHeader` directory declaring more than its section holds is a
        // malformed image, and CoreCLR's `CheckDirectory` rejects the whole directory rather
        // than reading the 16 valid bytes at its start. `Int32.MaxValue` also drives the
        // bound past what `int32` arithmetic could compute without overflowing.
        //
        // The bytes at that RVA are a perfectly good ReadyToRun header — identical to the
        // fixture above — so the *only* thing separating this answer from that one is the
        // bounds check.
        let overlong, state =
            withLoadedAssembly
                loggerFactory
                (baseImage "Overlong" |> withReadyToRunHeader (0x4Bu, System.Int32.MaxValue))
                state

        let state, overlongKind, overlongMachine =
            invokeGetPEKind loggerFactory prepared state overlong.Name.FullName

        overlongKind |> shouldEqual peILonly
        overlongMachine |> shouldEqual (int Machine.Amd64)

        // And the refusal, through the QCall rather than the decoder: a ReadyToRun image
        // whose source IL was architecture-specific.
        let specific, state =
            withLoadedAssembly
                loggerFactory
                (baseImage "Specific" |> withReadyToRunHeader (0x4Bu &&& ~~~0x1u, 16))
                state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetPEKind loggerFactory prepared state specific.Name.FullName
                |> ignore<IlMachineState * int * int>
            )

        exn.Message |> shouldContainText "READYTORUN_FLAG_PLATFORM_NEUTRAL_SOURCE"

    /// Builds a minimal assembly carrying exactly the manifest-row columns given. Hand-built
    /// rather than compiled because the display name is a function of all five columns and no
    /// compiler will emit most of these: there is no way to ask Roslyn for a processor
    /// architecture field, a `65535` major version, or a simple name containing a quote.
    let private displayNameImage
        (simpleName : string)
        (version : System.Version)
        (culture : string)
        (flags : int)
        (publicKey : byte[])
        : byte[]
        =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString (simpleName + ".dll"),
            metadata.GetOrAddGuid (System.Guid "0f9d8c7b-6a5e-4d3c-2b1a-0f9e8d7c6b5a"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString simpleName,
            version,
            (if System.String.IsNullOrEmpty culture then
                 Unchecked.defaultof<StringHandle>
             else
                 metadata.GetOrAddString culture),
            (if isNull publicKey then
                 Unchecked.defaultof<BlobHandle>
             else
                 metadata.GetOrAddBlob publicKey),
            enum<System.Reflection.AssemblyFlags> flags,
            System.Reflection.AssemblyHashAlgorithm.Sha1
        )
        |> ignore<AssemblyDefinitionHandle>

        // The real runtime declines to load an image with no `<Module>` type row, and it is
        // the oracle here, so give it one.
        metadata.AddTypeDefinition (
            Unchecked.defaultof<System.Reflection.TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                (if isNull publicKey then
                     CorFlags.ILOnly
                 else
                     CorFlags.ILOnly ||| CorFlags.StrongNameSigned)
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// The display name the *real* runtime reports for this image, or `None` if it declines to
    /// load it at all.
    ///
    /// This is the oracle these tests are built on, and it is the genuine article:
    /// `Assembly.FullName` goes straight to the QCall under test here, so a fixture's expected
    /// value is CoreCLR's own answer rather than a restatement of the implementation. That
    /// matters more than usual because the tempting wrong answer — `AssemblyName.FullName` —
    /// agrees with the right one on everything a compiler emits.
    ///
    /// Loaded into a collectible context so the test host is not permanently populated with
    /// these fixtures. Collectibility is guest-observable and so disqualifies a context from
    /// hosting a differential *execution* oracle, but nothing here executes: the assembly is
    /// only asked for its name.
    let private realRuntimeDisplayName (image : byte[]) : string option =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("displayNameOracle", isCollectible = true)

        try
            try
                use peImage = new MemoryStream (image)
                let assembly = context.LoadFromStream peImage
                Some assembly.FullName
            with
            | :? System.BadImageFormatException
            | :? System.IO.FileLoadException -> None
        finally
            context.Unload ()

    /// Every column combination that changes the display name, each flagged with whether
    /// `AssemblyName.FullName` — the display name PawPrint already holds, and the tempting
    /// implementation — agrees with CoreCLR there. Distinct simple names throughout, because
    /// PawPrint registers an assembly under its display name and same-named fixtures would
    /// displace one another.
    let private displayNameCases : (string * byte[] * bool) list =
        let v = System.Version (4, 3, 2, 1)

        [
            "plain", displayNameImage "WoofWare.DnPlain" v null 0x0 null, false
            // The culture column is reported verbatim, where `AssemblyName.CultureName`
            // normalises it to `en-GB`. First of the three reasons `Name.FullName` cannot
            // stand in for this QCall.
            "cultureRawCasing", displayNameImage "WoofWare.DnCulture" v "EN-gb" 0x0 null, true
            // Second reason: `AssemblyName.FullName` omits `processorArchitecture` altogether,
            // whatever the column says — so every one of these six diverges.
            "paMsil", displayNameImage "WoofWare.DnPaMsil" v null 0x10 null, true
            "paX86", displayNameImage "WoofWare.DnPaX86" v null 0x20 null, true
            "paAmd64", displayNameImage "WoofWare.DnPaAmd64" v null 0x40 null, true
            // `afPA_Mask` is a three-bit field, but CoreCLR bit-tests it in priority order, so
            // values overlapping a lower-numbered test answer to that instead. These three are
            // the whole reason the mapping is transcribed rather than reasoned out: 0x30 is
            // nominally IA64, 0x50 ARM and 0x60 ARM64, and none of them says so.
            "paIa64ReportsMsil", displayNameImage "WoofWare.DnPaIa64" v null 0x30 null, true
            "paArmReportsMsil", displayNameImage "WoofWare.DnPaArm" v null 0x50 null, true
            "paArm64ReportsX86", displayNameImage "WoofWare.DnPaArm64" v null 0x60 null, true
            "retargetable", displayNameImage "WoofWare.DnRetarget" v null 0x100 null, false
            // A `65535` major suppresses the entire `Version=` segment. It is a real value of
            // the row's `USHORT` column, not an impossible sentinel — and both renderers agree
            // on suppressing it.
            "majorVersion65535",
            displayNameImage "WoofWare.DnMaxVer" (System.Version (65535, 1, 2, 3)) null 0x0 null,
            false
            // Third reason: CoreCLR quotes with whichever of ' or " the name does not contain
            // and leaves that quote unescaped, where the BCL always double-quotes and
            // backslash-escapes.
            "nameWithDoubleQuote", displayNameImage "WoofWare.Dn\"Quote" v null 0x0 null, true
            "nameWithSingleQuote", displayNameImage "WoofWare.Dn'Quote" v null 0x0 null, true
            // The escapes the two renderers do agree on, so that the quoting cases above are
            // pinned as specifically about quotes rather than about escaping in general.
            "nameWithComma", displayNameImage "WoofWare.Dn,Comma" v null 0x0 null, false
            "nameWithTrailingSpace", displayNameImage "WoofWare.DnSpace " v null 0x0 null, false
        ]

    [<Test>]
    let ``GetFullName matches the real runtime for every column that changes the answer`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let mutable state = prepared.State

        for label, fixtureImage, _ in displayNameCases do
            let expected =
                realRuntimeDisplayName fixtureImage
                |> Option.defaultWith (fun () ->
                    failwith
                        $"the real runtime declined to load the %s{label} fixture, so it cannot be the oracle for it"
                )

            let loaded, next = withLoadedAssembly loggerFactory fixtureImage state

            let next, actual =
                invokeGetFullName loggerFactory prepared next loaded.Name.FullName

            state <- next

            if actual <> expected then
                failwith $"%s{label}: expected %s{expected} but got %s{actual}"

    [<Test>]
    let ``GetFullName is not AssemblyName.FullName`` () : unit =
        // Guards the reason the implementation reads raw columns rather than reusing the
        // display name PawPrint already has in hand, and keeps the fixture list honest in both
        // directions. A case marked as diverging that stopped diverging would quietly weaken
        // the test above — it could then be satisfied by the wrong implementation — and a case
        // marked as agreeing that started diverging would mean the fixture set had drifted
        // from what it claims to cover.
        for label, fixtureImage, expectedToDiverge in displayNameCases do
            let coreClr =
                realRuntimeDisplayName fixtureImage
                |> Option.defaultWith (fun () -> failwith $"the real runtime declined to load the %s{label} fixture")

            use peImage = new MemoryStream (fixtureImage)
            use peReader = new PEReader (peImage)

            let managed =
                peReader.GetMetadataReader().GetAssemblyDefinition().GetAssemblyName().FullName

            if expectedToDiverge && coreClr = managed then
                failwith
                    $"%s{label} is listed as a case where CoreCLR and AssemblyName disagree, but both now say %s{coreClr}"

            if not expectedToDiverge && coreClr <> managed then
                failwith
                    $"%s{label} is listed as a case where they agree, but CoreCLR says %s{coreClr} and AssemblyName says %s{managed}"

    [<Test>]
    let ``GetFullName reports the display name of a real assembly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let guest = prepared.State.ActiveAssembly prepared.EntryThread

        let state, guestName =
            invokeGetFullName loggerFactory prepared prepared.State guest.Name.FullName

        // The guest is unsigned and culture-neutral, so its display name is the four segments
        // every image has. Version comes from the `[assembly: AssemblyVersion]` in the source.
        guestName
        |> shouldEqual $"%s{guestAssemblyName}, Version=4.3.2.1, Culture=neutral, PublicKeyToken=null"

        // Corelib is the strong-named case, and its token is derived from the key rather than
        // stored: the manifest row carries the full 160-byte key.
        let corelib = prepared.BaseClassTypes.Corelib
        corelib.PublicKey.Length |> shouldEqual 160

        let _state, corelibName =
            invokeGetFullName loggerFactory prepared state corelib.Name.FullName

        corelibName |> shouldContainText ", PublicKeyToken=7cec85d7bea7798e"
        corelibName |> shouldContainText "System.Private.CoreLib, Version="

    [<Test>]
    let ``GetFullName reports a WindowsRuntime content type`` () : unit =
        // The one segment with no real-runtime oracle: CoreCLR refuses to *load* a
        // WindowsRuntime assembly at all ("The given assembly name was invalid"), so the
        // expectation is written down from `TextualIdentityParser::ToString` rather than
        // observed. PawPrint's loader has no such check, so a guest can reach it here.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let winRt =
            displayNameImage "WoofWare.DnWinRt" (System.Version (4, 3, 2, 1)) null 0x200 null

        realRuntimeDisplayName winRt |> shouldEqual None

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let loaded, state = withLoadedAssembly loggerFactory winRt prepared.State

        let _state, actual =
            invokeGetFullName loggerFactory prepared state loaded.Name.FullName

        actual
        |> shouldEqual
            "WoofWare.DnWinRt, Version=4.3.2.1, Culture=neutral, PublicKeyToken=null, ContentType=WindowsRuntime"

    [<Test>]
    let ``GetFullName on an unloaded assembly fails loudly`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invokeGetFullName
                    loggerFactory
                    prepared
                    prepared.State
                    "NotLoaded, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null"
                |> ignore<IlMachineState * string>
            )

        exn.Message |> shouldContainText "is not loaded"

    [<Test>]
    let ``GetFullName reports nothing at all for an empty simple name`` () : unit =
        // `TextualIdentityParser::ToString` clears its output and returns as soon as the
        // simple name is empty, so the display name is the empty string — not the remaining
        // segments with the name missing, which is what an implementation that simply appended
        // an empty name would produce.
        //
        // Like the WindowsRuntime case this has no real-runtime oracle: CoreCLR refuses to
        // load such an image at all. PawPrint's loader has no such check, so a guest can reach
        // it, and the expectation is read off upstream rather than observed.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let nameless = displayNameImage "" (System.Version (4, 3, 2, 1)) "EN-gb" 0x110 null

        realRuntimeDisplayName nameless |> shouldEqual None

        let image =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory image
        let loaded, state = withLoadedAssembly loggerFactory nameless prepared.State

        // Deliberately a fixture that would otherwise have plenty to say — a culture, a
        // processor architecture and the retargetable bit — so "empty" cannot be mistaken for
        // "nothing to report".
        let state, firstAddr =
            invokeStringQCall loggerFactory prepared "AssemblyNative_GetFullName" state loaded.Name.FullName

        let firstAddr =
            firstAddr
            |> Option.defaultWith (fun () -> failwith "handler left the StringHandleOnStack at null")

        assertIsString prepared.BaseClassTypes state firstAddr ""

        // `StringHandleOnStack::Set` goes through `StringObject::NewString`, which returns the
        // *shared* empty-string instance for a zero-length string. So two calls must hand back
        // the same reference here, where for any other length they must not — CoreCLR does not
        // intern QCall results, and a guest can see the difference with `ReferenceEquals`.
        let state, secondAddr =
            invokeStringQCall loggerFactory prepared "AssemblyNative_GetFullName" state loaded.Name.FullName

        secondAddr |> shouldEqual (Some firstAddr)

        // The contrast, on an assembly whose display name is not empty: freshly allocated each
        // time, so this pair must differ.
        let guest = state.ActiveAssembly prepared.EntryThread

        let state, guestFirst =
            invokeStringQCall loggerFactory prepared "AssemblyNative_GetFullName" state guest.Name.FullName

        let _state, guestSecond =
            invokeStringQCall loggerFactory prepared "AssemblyNative_GetFullName" state guest.Name.FullName

        guestFirst |> shouldNotEqual guestSecond

    [<Test>]
    let ``an assembly with a malformed public key never loads`` () : unit =
        // `publicKeyToken` hashes its blob without reproducing CoreCLR's
        // `StrongNameIsValidPublicKey` precondition, whose failure CoreCLR turns into a thrown
        // `CORSEC_E_INVALID_PUBLICKEY`. That is only sound because no such blob can reach it,
        // and this is what says so: PawPrint registers an assembly under its display name, and
        // computing that derives a token and rejects a key it cannot parse.
        //
        // Checked with the `afPublicKey` bit both set and clear, because CoreCLR force-sets it
        // for any non-empty AssemblyDef blob — so a flag-clear manifest is not a way to have
        // the blob treated as an already-computed token and skip the derivation.
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        // Not a well-formed `PublicKeyBlob`: no signature/hash algorithm identifiers and no
        // `PUBLICKEYBLOB` magic byte.
        let garbage = Array.init 32 byte

        let guestImage =
            Roslyn.compileAssembly guestAssemblyName OutputKind.ConsoleApplication [] [ guestSource ]

        let prepared = prepareGuest loggerFactory guestImage

        for label, flags in [ "flag clear", 0x0 ; "flag set", 0x1 ] do
            let image =
                displayNameImage
                    $"WoofWare.DnGarbage%s{label.Replace (' ', '-')}"
                    (System.Version (1, 0, 0, 0))
                    null
                    flags
                    garbage

            // The real runtime declines it too, so this is not a place the two disagree.
            realRuntimeDisplayName image |> shouldEqual None

            // Through `withLoadedAssembly`, which is the interpreter's own path: `Assembly.read`
            // by itself is lazy about the manifest row and does not look at the blob, so it is
            // deriving the *registration key* that rejects this — exactly the claim being
            // pinned.
            let exn =
                Assert.Throws<System.Security.SecurityException> (fun () ->
                    withLoadedAssembly loggerFactory image prepared.State
                    |> ignore<DumpedAssembly * IlMachineState>
                )

            exn.Message |> shouldContainText "public key"
