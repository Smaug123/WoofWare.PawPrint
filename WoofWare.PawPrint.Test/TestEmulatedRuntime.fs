namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Which runtime a run serves is read from the CoreLib it loaded: `EmulatedRuntime.classify`
/// keys on the image's `AssemblyVersion` major, and `Program.beginStartup` refuses a CoreLib whose
/// major is not supported. Separately, `EmulatedRuntime.pin` records which exact build our
/// validation was measured against; the drift tests here fail loudly when the CoreLib the suite
/// actually resolves is a different build, as happens when nixpkgs bumps the SDK. See
/// `EmulatedRuntime.fs` and the `sync-dotnet-runtime` skill.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedRuntime =

    let private assy = typeof<RunResult>.Assembly

    let private trivialGuest : string =
        """
public static class Program
{
    public static void Main() { }
}
"""

    let private coreLibFileName : string = "System.Private.CoreLib.dll"

    let private hostRuntimeDirs () : ImmutableArray<string> =
        DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

    /// The CoreLib PawPrint resolves when it starts a trivial guest along `runtimeDirs`.
    let private resolvedCoreLib (runtimeDirs : ImmutableArray<string>) : DumpedAssembly =
        let image = Roslyn.compile [ trivialGuest ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some "TrivialGuest.cs") peImage (HostConfig.Default runtimeDirs) with
        | Program.ProgramStartResult.Ready prepared -> prepared.BaseClassTypes.Corelib
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"the trivial guest ended during startup: %O{outcome}"

    /// The assembly-level `AssemblyInformationalVersionAttribute` value of `corelib`, verbatim.
    let private informationalVersion (corelib : DumpedAssembly) : string =
        let metadata = corelib.PeReader.GetMetadataReader ()

        let attributeTypeName (ctor : EntityHandle) : string * string =
            let ofTypeDefinition (handle : TypeDefinitionHandle) =
                let ty = metadata.GetTypeDefinition handle
                metadata.GetString ty.Namespace, metadata.GetString ty.Name

            match ctor.Kind with
            | HandleKind.MethodDefinition ->
                (metadata.GetMethodDefinition (MethodDefinitionHandle.op_Explicit ctor)).GetDeclaringType ()
                |> ofTypeDefinition
            | HandleKind.MemberReference ->
                let parent =
                    (metadata.GetMemberReference (MemberReferenceHandle.op_Explicit ctor)).Parent

                match parent.Kind with
                | HandleKind.TypeReference ->
                    let ty = metadata.GetTypeReference (TypeReferenceHandle.op_Explicit parent)
                    metadata.GetString ty.Namespace, metadata.GetString ty.Name
                | HandleKind.TypeDefinition -> ofTypeDefinition (TypeDefinitionHandle.op_Explicit parent)
                | other -> "", $"<attribute constructor parent of kind %O{other}>"
            | other -> "", $"<attribute constructor of kind %O{other}>"

        let attributes =
            metadata.GetAssemblyDefinition().GetCustomAttributes ()
            |> Seq.map metadata.GetCustomAttribute
            |> Seq.filter (fun attribute ->
                attributeTypeName attribute.Constructor = ("System.Reflection", "AssemblyInformationalVersionAttribute")
            )
            |> Seq.toList

        match attributes with
        | [ attribute ] ->
            let mutable reader = metadata.GetBlobReader attribute.Value
            // ECMA-335 II.23.3: a custom attribute blob begins with the prolog 0x0001.
            reader.ReadUInt16 () |> shouldEqual 1us
            reader.ReadSerializedString ()
        | other ->
            failwith
                $"expected exactly one AssemblyInformationalVersionAttribute on %s{corelib.DefinitionFullName}, found %d{other.Length}"

    /// The drift check proper: `corelib` is a supported runtime, and exactly the build pinned for it.
    let private assertMatchesPin (corelib : DumpedAssembly) : unit =
        let runtime =
            match EmulatedRuntime.classify corelib with
            | Ok runtime -> runtime
            | Error unsupported -> failwith $"the suite's own CoreLib is unsupported: %O{unsupported}"

        let found = RuntimeBuild.OfInformationalVersion (informationalVersion corelib)
        let pinned = (EmulatedRuntime.pin runtime).Build

        if found.Text <> pinned.Text then
            failwith
                $"The loaded CoreLib (%s{corelib.DefinitionFullName}, from %A{corelib.OriginalPath}) is build %s{found.Text}, but EmulatedRuntime.pin %O{runtime} says %s{pinned.Text}. Set that runtime's Build in WoofWare.PawPrint/EmulatedRuntime.fs to \"%s{found.Text}\" (and re-sync flake.nix and the dotnet-runtime-src pin; see the sync-dotnet-runtime skill)."

    [<Test>]
    let ``The CoreLib the suite resolves is the build EmulatedRuntime pins for its major`` () =
        assertMatchesPin (resolvedCoreLib (hostRuntimeDirs ()))

    [<Test>]
    let ``The pinned linux-x64 CoreLib is the build EmulatedRuntime pins for its major`` () =
        let frameworkDir = LinuxCoreLibFlavour.requireLinuxFramework ()

        let corelib =
            resolvedCoreLib (LinuxCoreLibFlavour.runtimeDirsPreferringLinux frameworkDir)

        // Otherwise this would silently re-check the host's CoreLib.
        corelib.OriginalPath
        |> shouldEqual (Some (LinuxCoreLibFlavour.corelibPath frameworkDir))

        assertMatchesPin corelib

    [<Test>]
    let ``A CoreLib major is supported exactly when it is some supported runtime's major`` () =
        // `AssemblyVersion` components are 16-bit, so this is every major an image can state.
        for coreLibMajor in 0 .. int UInt16.MaxValue do
            let expected =
                EmulatedRuntime.supported
                |> List.filter (fun runtime -> EmulatedRuntime.major runtime = coreLibMajor)

            match expected, EmulatedRuntime.ofCoreLibMajor coreLibMajor with
            | [], None -> ()
            | [ runtime ], Some classified when classified = runtime -> ()
            | expected, classified ->
                failwith
                    $"major %d{coreLibMajor}: supported runtimes with it are %A{expected}, classified as %A{classified}"

    [<Test>]
    let ``Net10 is the runtime of CoreLib major 10, and nothing else is supported`` () =
        EmulatedRuntime.supported |> shouldEqual [ EmulatedRuntime.Net10 ]
        EmulatedRuntime.ofCoreLibMajor 10 |> shouldEqual (Some EmulatedRuntime.Net10)

    [<Test>]
    let ``A runtime build is the informational version up to its build metadata`` () =
        let noPlus (s : NonEmptyString) : bool = not (s.Get.Contains '+')

        let property (core : NonEmptyString) (metadata : string) : bool =
            let withMetadata =
                RuntimeBuild.OfInformationalVersion (
                    core.Get + "+" + (metadata |> Option.ofObj |> Option.defaultValue "")
                )

            let without = RuntimeBuild.OfInformationalVersion core.Get
            withMetadata.Text = core.Get && without.Text = core.Get

        let cores = ArbMap.defaults |> ArbMap.arbitrary<NonEmptyString> |> Arb.filter noPlus

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll cores (fun core -> Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<string>) (property core))
        )

    /// A copy of the host's CoreLib whose AssemblyDef row states `major`, written into `dir`.
    ///
    /// Assembly binding is by simple name and ignores the referenced version, so a directory
    /// holding this at the head of `DotnetRuntimeDirs` is where the guest's CoreLib comes from.
    let private writeCoreLibWithMajor (major : uint16) (dir : string) : string =
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

        let path = Path.Combine (dir, coreLibFileName)
        File.WriteAllBytes (path, bytes)

        use peReader = new PEReader (ImmutableArray.Create<byte> bytes)

        let patchedDefinition = peReader.GetMetadataReader().GetAssemblyDefinition ()

        patchedDefinition.Version.Major |> shouldEqual (int major)

        path

    [<TestCase 0us>]
    [<TestCase 9us>]
    [<TestCase 11us>]
    [<TestCase 65535us>]
    let ``Startup refuses a CoreLib whose major is not supported, naming what is`` (major : uint16) =
        let dir =
            Path.Combine (Path.GetTempPath (), "PawPrintUnsupportedCoreLib", Guid.NewGuid().ToString ("N"))

        Directory.CreateDirectory dir |> ignore<DirectoryInfo>

        try
            let patched = writeCoreLibWithMajor major dir

            let runtimeDirs =
                seq {
                    yield dir
                    yield! hostRuntimeDirs ()
                }
                |> ImmutableArray.CreateRange

            let image = Roslyn.compile [ trivialGuest ]
            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            use peImage = new MemoryStream (image)

            let refusal =
                try
                    Program.beginStartup loggerFactory (Some "TrivialGuest.cs") peImage (HostConfig.Default runtimeDirs)
                    |> ignore<Program.Startup>

                    None
                with :? UnsupportedRuntimeException as e ->
                    Some e

            match refusal with
            | None -> failwith $"startup accepted a CoreLib stating major %d{major}"
            | Some refusal ->
                refusal.Unsupported.FoundMajor |> shouldEqual (int major)
                refusal.Unsupported.CoreLibPath |> shouldEqual (Some patched)
                refusal.DotnetRuntimeDirs |> shouldEqual runtimeDirs
                refusal.Message |> shouldContainText $"is .NET major %d{major}"
                refusal.Message |> shouldContainText "supports CoreLib majors: 10 (net10.0)"
        finally
            Directory.Delete (dir, true)
