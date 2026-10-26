namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The default-context binder's version and request rules, and its binding and image refusals
/// against temporary runtime directories, independently of the interpreter's native-call handlers.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyBinding =

    let private unspecified = RequestedAssemblyVersion.Unspecified

    /// CoreLib spellings measured on .NET 10, including the comma-led suffix that its
    /// `BaseAssemblySpec::IsCoreLib` accepts and the similar names it does not.
    [<TestCase("System.Private.CoreLib", true)>]
    [<TestCase("system.private.corelib", true)>]
    [<TestCase("System.Private.CoreLib,garbage", true)>]
    [<TestCase("SYSTEM.PRIVATE.CORELIB, Version=9", true)>]
    [<TestCase("System.Private.CoreLib.dll", true)>]
    [<TestCase("system.private.corelib.DLL", true)>]
    [<TestCase("System.Private.CoreLib.dll,x", false)>]
    [<TestCase("System.Private.CoreLibX", false)>]
    [<TestCase("System.Private.CoreLib2", false)>]
    [<TestCase("System.Private.CoreLi", false)>]
    let ``isCoreLibName is the loose CoreLib reference check`` (name : string) (expected : bool) : unit =
        AssemblyBinding.isCoreLibName name |> shouldEqual expected

    let private requested
        (major : uint16)
        (minor : uint16)
        (build : uint16)
        (revision : uint16)
        : RequestedAssemblyVersion
        =
        {
            Major = major
            Minor = minor
            Build = build
            Revision = revision
        }

    /// Rows measured on .NET 10 against the framework's `10.0.0.0`, plus the major-only
    /// requests a display name cannot express.
    [<TestCase(10us, 0us, 0us, 0us, true)>]
    [<TestCase(9us, 9us, 9us, 9us, true)>]
    [<TestCase(10us, 0us, 0us, 1us, false)>]
    [<TestCase(10us, 0us, 1us, 0xFFFFus, false)>]
    [<TestCase(10us, 1us, 0xFFFFus, 0xFFFFus, false)>]
    [<TestCase(10us, 0us, 0xFFFFus, 0xFFFFus, true)>]
    [<TestCase(10us, 0xFFFFus, 0xFFFFus, 0xFFFFus, true)>]
    [<TestCase(11us, 0xFFFFus, 0xFFFFus, 0xFFFFus, false)>]
    [<TestCase(0xFFFFus, 0xFFFFus, 0xFFFFus, 0xFFFFus, true)>]
    [<TestCase(0xFFFFus, 99us, 99us, 99us, true)>]
    let ``isCompatibleVersion against 10.0.0.0``
        (major : uint16)
        (minor : uint16)
        (build : uint16)
        (revision : uint16)
        (expected : bool)
        : unit
        =
        AssemblyBinding.isCompatibleVersion (requested major minor build revision) (Version (10, 0, 0, 0))
        |> shouldEqual expected

    /// A found component of 65535 is itself "unspecified", and a request that names that
    /// component fails against it however small the request is.
    [<Test>]
    let ``a specific request does not match an unspecified found component`` () : unit =
        AssemblyBinding.isCompatibleVersion (requested 1us 0us 0xFFFFus 0xFFFFus) (Version (1, 0xFFFF, 0, 0))
        |> shouldEqual false

        AssemblyBinding.isCompatibleVersion (requested 1us 0xFFFFus 0xFFFFus 0xFFFFus) (Version (1, 0xFFFF, 0, 0))
        |> shouldEqual true

    /// The rule restated: the request's components up to its first unspecified one, compared
    /// lexicographically against the found version's first as-many components, are no greater;
    /// and none of those found components is itself unspecified.
    let private lexicographic (req : RequestedAssemblyVersion) (found : Version) : bool =
        let prefix =
            [ req.Major ; req.Minor ; req.Build ; req.Revision ]
            |> List.takeWhile (fun c -> c <> unspecified)

        let foundPrefix =
            [ found.Major ; found.Minor ; found.Build ; found.Revision ]
            |> List.map uint16
            |> List.truncate prefix.Length

        if foundPrefix |> List.exists (fun c -> c = unspecified) then
            // A specific request against an unspecified found component fails, but only if the
            // comparison reaches it: an earlier strict inequality decides first.
            let decidedEarlier =
                List.zip prefix foundPrefix
                |> List.tryPick (fun (r, f) ->
                    if f = unspecified then Some false
                    elif r < f then Some true
                    elif r > f then Some false
                    else None
                )

            decidedEarlier |> Option.defaultValue true
        else
            compare prefix foundPrefix <= 0

    [<Test>]
    let ``isCompatibleVersion is the lexicographic comparison of the specified prefix`` () : unit =
        let component' =
            Gen.frequency [ 5, Gen.choose (0, 3) |> Gen.map uint16 ; 1, Gen.constant unspecified ]

        let gen =
            gen {
                let! rm = component'
                let! rn = component'
                let! rb = component'
                let! rr = component'
                let! fm = component'
                let! fn = component'
                let! fb = component'
                let! fr = component'
                return requested rm rn rb rr, Version (int fm, int fn, int fb, int fr)
            }

        let mutable compatible = 0
        let mutable incompatible = 0

        let property (req : RequestedAssemblyVersion, found : Version) : unit =
            let expected = lexicographic req found

            if expected then
                compatible <- compatible + 1
            else
                incompatible <- incompatible + 1

            AssemblyBinding.isCompatibleVersion req found |> shouldEqual expected

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)
        compatible |> shouldBeGreaterThan 200
        incompatible |> shouldBeGreaterThan 200

    /// The image the tests copy about: a small one, because `Assembly.readFile` keeps every parse
    /// it makes, keyed by full path, for the life of the process, and each test here reads a
    /// fresh copy. A copy of the ten-megabyte test assembly per read is more than the test host's
    /// capped heap can keep.
    let private imagePath : string =
        typeof<WoofWare.PawPrint.Logging.LoggingConfig>.Assembly.Location

    /// A directory holding the image under a name of the test's choosing, so that the probe reads
    /// a real image without the suite depending on any other file.
    let private withRuntimeDir (relativePath : string) (body : string -> unit) : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        let target = Path.Combine (root, relativePath)

        Directory.CreateDirectory (Path.GetDirectoryName target)
        |> ignore<DirectoryInfo>

        File.Copy (imagePath, target)

        try
            body root
        finally
            Directory.Delete (root, true)

    let private imageSimpleName = "WoofWare.PawPrint.Logging"

    /// `TestCandidateRefMatchesDef`: the file was chosen by its name, and the manifest inside it
    /// must agree before it answers.
    [<Test>]
    let ``a file whose manifest names another assembly does not bind`` () : unit =
        withRuntimeDir
            "Alias.dll"
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request : AssemblyLoadRequest =
                    {
                        SimpleName = "Alias"
                        Version = requested unspecified unspecified unspecified unspecified
                    }

                match AssemblyBinding.tryBind loggerFactory [ root ] request LoadedAssemblies.empty with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound (_, bound) -> failwith $"expected NotFound, bound %s{bound.Name.FullName}"
            )

    [<Test>]
    let ``binding registers the assembly once and answers the same instance thereafter`` () : unit =
        withRuntimeDir
            (imageSimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request (major : uint16) : AssemblyLoadRequest =
                    {
                        SimpleName = imageSimpleName.ToUpperInvariant ()
                        Version = requested major unspecified unspecified unspecified
                    }

                // Too high a version: not found, and nothing registered.
                match AssemblyBinding.tryBind loggerFactory [ root ] (request 999us) LoadedAssemblies.empty with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound _ -> failwith "expected NotFound"

                let assemblies, first =
                    match
                        AssemblyBinding.tryBind loggerFactory [ root ] (request unspecified) LoadedAssemblies.empty
                    with
                    | AssemblyBindResult.Bound (assemblies, assy) -> assemblies, assy
                    | AssemblyBindResult.NotFound -> failwith "expected Bound"

                assemblies.DefinitionNamesInLoadOrder
                |> Seq.toList
                |> shouldEqual [ first.Name.FullName ]

                // The second bind is answered from the context, not the disk: the directory is gone.
                Directory.Delete (root, true)
                Directory.CreateDirectory root |> ignore<DirectoryInfo>

                match AssemblyBinding.tryBind loggerFactory [ root ] (request unspecified) assemblies with
                | AssemblyBindResult.Bound (again, second) ->
                    Object.ReferenceEquals (first, second) |> shouldEqual true

                    again.DefinitionNamesInLoadOrder
                    |> Seq.toList
                    |> shouldEqual [ first.Name.FullName ]
                | AssemblyBindResult.NotFound -> failwith "expected Bound from the load context"

                // A version the loaded one cannot satisfy is a miss even though it is loaded.
                match AssemblyBinding.tryBind loggerFactory [ root ] (request 999us) assemblies with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound _ -> failwith "expected NotFound against the loaded version"
            )

    let private plainRequest (name : string) : AssemblyLoadRequest =
        {
            SimpleName = name
            Version = requested unspecified unspecified unspecified unspecified
        }

    [<Test>]
    let ``isNeutralCulture is the binder's rule`` () : unit =
        for culture, expected in
            [
                None, true
                Some "", true
                Some "neutral", true
                Some "NEUTRAL", true
                Some "Neutral", true
                Some "fr", false
                Some "neutral2", false
                Some "en-US", false
            ] do
            AssemblyBinding.isNeutralCulture culture |> shouldEqual expected

    /// `BaseAssemblySpec::IsCoreLibSatellite`: the name and the full key must both match.
    [<Test>]
    let ``isCoreLibSatellite needs the name and CoreLib's full key`` () : unit =
        let coreLibKey = typeof<obj>.Assembly.GetName().GetPublicKey ()
        let otherKey = Array.append coreLibKey [| 0uy |]

        for name in
            [
                "System.Private.CoreLib.resources"
                "system.private.corelib.RESOURCES"
                "System.Private.CoreLib.resources, Culture=fr"
            ] do
            AssemblyBinding.isCoreLibSatellite name (Some coreLibKey) coreLibKey
            |> shouldEqual true

        AssemblyBinding.isCoreLibSatellite "System.Private.CoreLib.resources" None coreLibKey
        |> shouldEqual false

        AssemblyBinding.isCoreLibSatellite "System.Private.CoreLib.resources" (Some otherKey) coreLibKey
        |> shouldEqual false

        for name in
            [
                "System.Private.CoreLib.resourcesX"
                "System.Private.CoreLib"
                "System.Private.CoreLib.resource"
            ] do
            AssemblyBinding.isCoreLibSatellite name (Some coreLibKey) coreLibKey
            |> shouldEqual false

    /// Only the public-key bit is admitted; every other bit is refused by name rather than
    /// bound, because it either makes the binder's unmodelled caches observable or has an effect
    /// nobody has measured.
    [<TestCase(0x0, true)>]
    [<TestCase(0x1, true)>]
    [<TestCase(0x10, false)>] // MSIL
    [<TestCase(0x11, false)>] // MSIL, with a full key
    [<TestCase(0x20, false)>] // x86
    [<TestCase(0x30, false)>] // IA64
    [<TestCase(0x40, false)>] // AMD64
    [<TestCase(0x50, false)>] // ARM
    [<TestCase(0x60, false)>] // ARM64
    [<TestCase(0x70, false)>] // NoPlatform
    [<TestCase(0x100, false)>] // Retargetable
    [<TestCase(0x200, false)>] // ContentType=WindowsRuntime
    [<TestCase(0x400, false)>] // a reserved content type
    [<TestCase(0x4000, false)>] // EnableJITcompileOptimizer
    [<TestCase(0x8000, false)>] // EnableJITcompileTracking
    let ``unsupportedFlags admits only the public-key bit`` (flags : int) (admitted : bool) : unit =
        AssemblyBinding.unsupportedFlags flags |> Option.isNone |> shouldEqual admitted

    /// Measured on .NET 10: `/tmp/target`, `../target` and `a/b` are each reported not found.
    /// The file here exists at exactly the path the rooted name would reach, so a probe that
    /// followed it would bind.
    [<Test>]
    let ``a name with a separator, or a rooted one, is a miss without a probe`` () : unit =
        withRuntimeDir
            "target.dll"
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                for name in [ Path.Combine (root, "target") ; "../target" ; "a/b" ; "sub\\target" ] do
                    match
                        AssemblyBinding.tryBind loggerFactory [ root ] (plainRequest name) LoadedAssemblies.empty
                    with
                    | AssemblyBindResult.NotFound -> ()
                    | AssemblyBindResult.Bound (_, bound) -> failwith $"'%s{name}' bound %s{bound.Name.FullName}"
            )

    /// A copy of the small fixture image with bytes replaced at the given offset in its
    /// manifest row, written under the given file name.
    let private writeImageWithManifestPatch
        (root : string)
        (fileName : string)
        (offsetInRow : int)
        (patch : byte[])
        : unit
        =
        let bytes = File.ReadAllBytes imagePath

        let rowFileOffset =
            use pe = new PEReader (new MemoryStream (bytes))
            let reader = pe.GetMetadataReader ()

            pe.PEHeaders.MetadataStartOffset
            + reader.GetTableMetadataOffset TableIndex.Assembly

        patch |> Array.iteri (fun i b -> bytes.[rowFileOffset + offsetInRow + i] <- b)

        File.WriteAllBytes (Path.Combine (root, fileName), bytes)

    let private writeImageWithManifestFlags (root : string) (fileName : string) (flags : uint32) : unit =
        writeImageWithManifestPatch root fileName 12 (BitConverter.GetBytes flags)

    /// `AssemblySpec::PopulateAssemblyNameData` hands the binder `0.0.0.0` for a request whose
    /// major is unspecified, and a found component of 65535 satisfies no specific request: so an
    /// unversioned request misses an assembly versioned `65535.0.0.0`. Explicitly naming
    /// that major still means "unspecified", so it misses too.
    [<Test>]
    let ``an unversioned request misses an assembly whose major is 65535`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            // The major version is the row's first column after `HashAlgId`.
            writeImageWithManifestPatch root (imageSimpleName + ".dll") 4 (BitConverter.GetBytes 0xFFFFus)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            match
                AssemblyBinding.tryBind loggerFactory [ root ] (plainRequest imageSimpleName) LoadedAssemblies.empty
            with
            | AssemblyBindResult.NotFound -> ()
            | AssemblyBindResult.Bound (_, bound) -> failwith $"expected NotFound, bound %s{bound.Name.FullName}"

            let versioned =
                { plainRequest imageSimpleName with
                    Version = requested 0xFFFFus unspecified unspecified unspecified
                }

            match AssemblyBinding.tryBind loggerFactory [ root ] versioned LoadedAssemblies.empty with
            | AssemblyBindResult.NotFound -> ()
            | AssemblyBindResult.Bound (_, bound) ->
                // A major of 65535 is itself "unspecified", so it cannot be asked for.
                failwith $"expected NotFound for the unspecifiable major too, bound %s{bound.Name.FullName}"
        finally
            Directory.Delete (root, true)

    let private expectRefusal (containing : string) (bind : unit -> AssemblyBindResult) : unit =
        let exn = Assert.Throws<Exception> (fun () -> bind () |> ignore<AssemblyBindResult>)

        exn.Message |> shouldContainText containing

    /// A probed file whose manifest names it with a dotless i where the request has `gi`: the
    /// names have the same length, CoreCLR's per-character folding may equate them where an
    /// ordinal comparison does not, and that folding is not modelled.
    [<Test>]
    let ``a probed image whose manifest name is not ASCII is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let bytes = File.ReadAllBytes imagePath

            let nameOffset, nameLength =
                use pe = new PEReader (new MemoryStream (bytes))
                let reader = pe.GetMetadataReader ()
                let name = reader.GetAssemblyDefinition().Name

                pe.PEHeaders.MetadataStartOffset
                + reader.GetHeapMetadataOffset HeapIndex.String
                + MetadataTokens.GetHeapOffset (StringHandle.op_Implicit name : Handle),
                (reader.GetString name).Length

            // "...Logging" becomes "...Log\u0131ng": the two bytes of `gi` become the two UTF-8
            // bytes of U+0131, so nothing else in the heap moves.
            let gi =
                [ nameOffset .. nameOffset + nameLength - 2 ]
                |> List.findBack (fun i -> bytes.[i] = byte 'g' && bytes.[i + 1] = byte 'i')

            bytes.[gi] <- 0xC4uy
            bytes.[gi + 1] <- 0xB1uy

            let requested = "WoofWare.PawPrint.Loging"
            File.WriteAllBytes (Path.Combine (root, requested + ".dll"), bytes)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "not ASCII"
                (fun () ->
                    AssemblyBinding.tryBind loggerFactory [ root ] (plainRequest requested) LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)

    /// An image marked 32-bit-required alone is x86-only, which CoreCLR refuses to load on any of
    /// the 64-bit platforms PawPrint simulates.
    [<Test>]
    let ``a 32-bit-required image is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let bytes = File.ReadAllBytes imagePath

            // `IMAGE_COR20_HEADER.Flags` follows `cb`, the two runtime-version words and the
            // eight-byte `MetaData` directory.
            let flagsOffset =
                use pe = new PEReader (new MemoryStream (bytes))
                pe.PEHeaders.CorHeaderStartOffset + 16

            let flags = BitConverter.ToUInt32 (bytes, flagsOffset) ||| 0x2u &&& ~~~0x20000u

            BitConverter.GetBytes flags
            |> Array.iteri (fun i b -> bytes.[flagsOffset + i] <- b)

            File.WriteAllBytes (Path.Combine (root, imageSimpleName + ".dll"), bytes)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "32-bit"
                (fun () ->
                    AssemblyBinding.tryBind
                        loggerFactory
                        [ root ]
                        (plainRequest imageSimpleName)
                        LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)

    [<Test>]
    let ``forBinding zeroes a version whose major is unspecified, and only then`` () : unit =
        RequestedAssemblyVersion.forBinding (requested unspecified 1us 2us 3us)
        |> shouldEqual (requested 0us 0us 0us 0us)

        RequestedAssemblyVersion.forBinding (requested 10us unspecified unspecified unspecified)
        |> shouldEqual (requested 10us unspecified unspecified unspecified)

    [<Test>]
    let ``mayFoldToSameName flags a non-ASCII name of the request's length`` () : unit =
        AssemblyBinding.mayFoldToSameName "B\u0131ndProbe" "BindProbe"
        |> shouldEqual true

        AssemblyBinding.mayFoldToSameName "B\u0131ndProbe" "Bind" |> shouldEqual false
        AssemblyBinding.mayFoldToSameName "BindProbe" "BINDPROBE" |> shouldEqual false
        AssemblyBinding.mayFoldToSameName "OtherName" "BindProbe" |> shouldEqual false

    /// CoreCLR refuses any manifest that sets a content-type bit, the reserved values as well as
    /// WindowsRuntime; `AssemblyName.ContentType` folds the reserved ones back to `Default`, so
    /// the check must read the row.
    [<TestCase(0x200u)>]
    [<TestCase(0x400u)>]
    [<TestCase(0x800u)>]
    let ``a manifest with a content-type bit is refused`` (contentType : uint32) : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            writeImageWithManifestFlags root (imageSimpleName + ".dll") contentType
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            let refused =
                try
                    AssemblyBinding.tryBind loggerFactory [ root ] (plainRequest imageSimpleName) LoadedAssemblies.empty
                    |> ignore<AssemblyBindResult>

                    false
                with e when e.Message.Contains "set a content type" ->
                    true

            refused |> shouldEqual true
        finally
            Directory.Delete (root, true)

    /// CoreCLR's binder never consults a dynamic assembly: `Assembly.Load` of a name only an
    /// `AssemblyBuilder` defines is not found, as a reference naming it is not
    /// (`LoadedAssemblies.TryResolveReference`).
    [<Test>]
    let ``a dynamic assembly does not answer a request for its name`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()

        let dynamic =
            use stream = File.OpenRead imagePath
            AssemblyApi.read loggerFactory None stream

        let assemblies =
            match LoadedAssemblies.empty.WithDynamicAssembly dynamic with
            | Ok assemblies -> assemblies
            | Error existing -> failwith $"unexpected collision with %s{existing.DefinitionFullName}"

        match AssemblyBinding.tryBind loggerFactory [] (plainRequest imageSimpleName) assemblies with
        | AssemblyBindResult.NotFound -> ()
        | AssemblyBindResult.Bound (_, bound) -> failwith $"expected NotFound, bound %s{bound.Name.FullName}"

    /// `PEAssembly::ValidateForExecution` refuses an image whose manifest carries
    /// `ReferenceAssemblyAttribute`, raising `BadImageFormatException`, which this binder does not
    /// report.
    [<Test>]
    let ``a probed reference assembly is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let image =
                Roslyn.compileAssembly
                    "RefOnly"
                    Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                    []
                    [
                        "[assembly: System.Runtime.CompilerServices.ReferenceAssembly] public class C { }"
                    ]

            File.WriteAllBytes (Path.Combine (root, "RefOnly.dll"), image)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "reference assembly"
                (fun () ->
                    AssemblyBinding.tryBind loggerFactory [ root ] (plainRequest "RefOnly") LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)

    /// `PEAssembly::ValidateForExecution` refuses a manifest whose processor-architecture field is
    /// `afPA_NoPlatform` (0x70), raising `BadImageFormatException`, which this binder does not report.
    [<Test>]
    let ``a manifest marked NoPlatform is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            writeImageWithManifestFlags root (imageSimpleName + ".dll") 0x70u
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "NoPlatform"
                (fun () ->
                    AssemblyBinding.tryBind
                        loggerFactory
                        [ root ]
                        (plainRequest imageSimpleName)
                        LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)

    /// An IL image built for one machine binds only in a process of that architecture, which the
    /// binder does not model; CoreCLR reports one built for another as not found.
    [<Test>]
    let ``a probed image built for one machine is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let bytes = File.ReadAllBytes imagePath

            let machineOffset =
                use pe = new PEReader (new MemoryStream (bytes))
                pe.PEHeaders.CoffHeaderStartOffset

            BitConverter.GetBytes (uint16 Machine.Amd64)
            |> Array.iteri (fun i b -> bytes.[machineOffset + i] <- b)

            File.WriteAllBytes (Path.Combine (root, imageSimpleName + ".dll"), bytes)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "built for the machine"
                (fun () ->
                    AssemblyBinding.tryBind
                        loggerFactory
                        [ root ]
                        (plainRequest imageSimpleName)
                        LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)

    /// A file named with a dotless i where the request has `i`: CoreCLR's map of trusted platform
    /// assemblies may fold the two together, where an ordinal comparison of file names does not,
    /// so neither "not found" nor a bind is known to be right.
    [<Test>]
    let ``a runtime-dir file whose non-ASCII name may fold to the request is refused`` () : unit =
        withRuntimeDir
            "BındProbe.dll"
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                expectRefusal
                    "not ASCII"
                    (fun () ->
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            (plainRequest "BindProbe")
                            LoadedAssemblies.empty
                    )
            )

    /// An I386 image that is not IL-only is x86 to CoreCLR (`TranslatePEToArchitectureType`), not
    /// architecture-neutral, so it binds only in an x86 process.
    [<Test>]
    let ``a probed I386 image that is not IL-only is refused`` () : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let bytes = File.ReadAllBytes imagePath

            // The COR header's `Flags` follow `cb` (4), the runtime version (2 + 2) and the
            // metadata directory (8).
            let flagsOffset, flags =
                use pe = new PEReader (new MemoryStream (bytes))
                pe.PEHeaders.CorHeaderStartOffset + 16, pe.PEHeaders.CorHeader.Flags

            BitConverter.GetBytes (uint32 (flags &&& ~~~CorFlags.ILOnly))
            |> Array.iteri (fun i b -> bytes.[flagsOffset + i] <- b)

            File.WriteAllBytes (Path.Combine (root, imageSimpleName + ".dll"), bytes)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            expectRefusal
                "built for the machine"
                (fun () ->
                    AssemblyBinding.tryBind
                        loggerFactory
                        [ root ]
                        (plainRequest imageSimpleName)
                        LoadedAssemblies.empty
                )
        finally
            Directory.Delete (root, true)
