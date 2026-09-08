namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The pure halves of binding an assembly by display name: the version rule and the display
/// name a failure reports. The bind itself is exercised end to end by
/// `sourcesPure/AssemblyLoadByName.cs`, `AssemblyLoadMissing.cs` and
/// `AssemblyLoadVersionCompatibility.cs` against the real runtime.
/// Names the test assembly, whose image the probe tests copy about.
type private BindingTestMarker = class end

[<TestFixture>]
module TestAssemblyBinding =

    let private unspecified = RequestedAssemblyVersion.Unspecified

    let private requested (major : uint16) (minor : uint16) (build : uint16) (revision : uint16) =
        {
            Major = major
            Minor = minor
            Build = build
            Revision = revision
        }

    /// Rows measured on .NET 10 against the framework's `10.0.0.0`
    /// (`sourcesPure/AssemblyLoadVersionCompatibility.cs` holds the same table, run there), plus
    /// the major-only rows a display name cannot express and so only this test can reach.
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
        =
        AssemblyBinding.isCompatibleVersion (requested major minor build revision) (Version (10, 0, 0, 0))
        |> shouldEqual expected

    /// A found component of 65535 is itself "unspecified", and a request that names that
    /// component fails against it however small the request is.
    [<Test>]
    let ``a specific request does not match an unspecified found component`` () =
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
    let ``isCompatibleVersion is the lexicographic comparison of the specified prefix`` () =
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

        let property (req : RequestedAssemblyVersion, found : Version) =
            AssemblyBinding.isCompatibleVersion req found = lexicographic req found

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

    let private token =
        [| 0xb0uy ; 0x3fuy ; 0x5fuy ; 0x7fuy ; 0x11uy ; 0xd5uy ; 0x0auy ; 0x3auy |]

    /// Every row is the `FileName` of a `FileNotFoundException` measured on .NET 10 for the
    /// request it spells.
    [<Test>]
    let ``display name of a request`` () =
        let display
            (name : string)
            (v : RequestedAssemblyVersion)
            (culture : string)
            (t : byte[] option)
            (flags : int)
            =
            NativeRuntimeAssembly.displayNameWithToken name (RequestedAssemblyVersion.toVersion v) culture t flags

        display "No.Such.Assembly" (requested unspecified unspecified unspecified unspecified) "" None 0
        |> shouldEqual "No.Such.Assembly, Culture=neutral, PublicKeyToken=null"

        display "No.Such" (requested 1us 2us unspecified unspecified) "" None 0
        |> shouldEqual "No.Such, Version=1.2.65535.65535, Culture=neutral, PublicKeyToken=null"

        display "No.Such.Assembly" (requested 1us 2us 3us 4us) "" (Some token) 0
        |> shouldEqual "No.Such.Assembly, Version=1.2.3.4, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"

        // afRetargetable
        display "No.Such" (requested 1us 0us 0us 0us) "" (Some token) 0x100
        |> shouldEqual "No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes"

        // afPA_x86
        display "No.Such" (requested unspecified unspecified unspecified unspecified) "" None 0x20
        |> shouldEqual "No.Such, Culture=neutral, PublicKeyToken=null, processorArchitecture=x86"

        display "System.Security.Claims" (requested 1us 0us 0us 0us) "fr" None 0
        |> shouldEqual "System.Security.Claims, Version=1.0.0.0, Culture=fr, PublicKeyToken=null"

        display "No Such" (requested unspecified unspecified unspecified unspecified) "" None 0
        |> shouldEqual "No Such, Culture=neutral, PublicKeyToken=null"

        // afContentType_WindowsRuntime
        display "No.Such" (requested unspecified unspecified unspecified unspecified) "" None 0x200
        |> shouldEqual "No.Such, Culture=neutral, PublicKeyToken=null, ContentType=WindowsRuntime"

    let private microsoftKey =
        Convert.FromHexString
            "002400000480000094000000060200000024000052534131000400000100010007d1fa57c4aed9f0a32e84aa0faefd0de9e8fd6aec8f87fb03766c834c99921eb23be79ad9d5dcc1dd9ad236132102900b723cf980957fc4e177108fc607774f29e8320e92ea05ece4e821c0a5efe8f1645c4c0c93c1ab99285d622caa652c1dfad63d745d6f2de5f17e5eaf0fc4963d261c8a12436518206dc093344d5ad293"

    let private ecmaKey = Convert.FromHexString "00000000000000000400000000000000"

    /// Both tokens are what .NET 10 reports a missing assembly under when asked for by the
    /// corresponding full key.
    [<Test>]
    let ``token of a full public key`` () =
        NativeRuntimeAssembly.publicKeyToken microsoftKey
        |> Convert.ToHexString
        |> fun s -> s.ToLowerInvariant ()
        |> shouldEqual "b03f5f7f11d50a3a"

        NativeRuntimeAssembly.publicKeyToken ecmaKey
        |> Convert.ToHexString
        |> fun s -> s.ToLowerInvariant ()
        |> shouldEqual "b77a5c561934e089"

    [<Test>]
    let ``StrongNameIsValidPublicKey`` () =
        NativeRuntimeAssembly.isValidPublicKey microsoftKey |> shouldEqual true
        NativeRuntimeAssembly.isValidPublicKey ecmaKey |> shouldEqual true
        // Measured: `PublicKey=0011` is "Invalid assembly public key."
        NativeRuntimeAssembly.isValidPublicKey [| 0x00uy ; 0x11uy |]
        |> shouldEqual false
        // A header whose key length disagrees with the blob.
        NativeRuntimeAssembly.isValidPublicKey (Array.append microsoftKey [| 0uy |])
        |> shouldEqual false
        // A key that does not announce itself as a PUBLICKEYBLOB.
        let notAKeyBlob = Array.copy microsoftKey
        notAKeyBlob.[12] <- 7uy
        NativeRuntimeAssembly.isValidPublicKey notAKeyBlob |> shouldEqual false
        // A hash algorithm of the wrong class.
        let wrongHashClass = Array.copy microsoftKey
        wrongHashClass.[5] <- 0x20uy
        NativeRuntimeAssembly.isValidPublicKey wrongHashClass |> shouldEqual false
        // An unspecified hash algorithm is allowed.
        let noHash = Array.copy microsoftKey
        noHash.[4] <- 0uy
        noHash.[5] <- 0uy
        NativeRuntimeAssembly.isValidPublicKey noHash |> shouldEqual true

    /// A directory holding the test assembly itself under a name of the test's choosing, so that
    /// the probe reads a real image without the suite depending on any other file.
    let private withRuntimeDir (relativePath : string) (body : string -> unit) : unit =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        let target = Path.Combine (root, relativePath)

        Directory.CreateDirectory (Path.GetDirectoryName target)
        |> ignore<DirectoryInfo>

        File.Copy (typeof<BindingTestMarker>.Assembly.Location, target)

        try
            body root
        finally
            Directory.Delete (root, true)

    let private testAssemblySimpleName = "WoofWare.PawPrint.Test"

    [<Test>]
    let ``a satellite is probed in the culture's subdirectory, and only there`` () =
        withRuntimeDir
            (Path.Combine ("fr", testAssemblySimpleName + ".dll"))
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] None testAssemblySimpleName
                |> Option.map (fun assy -> assy.Name.Name)
                |> shouldEqual None

                AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] (Some "fr") testAssemblySimpleName
                |> Option.map (fun assy -> assy.Name.Name)
                |> shouldEqual (Some testAssemblySimpleName)
            )

    [<Test>]
    let ``a culture with no satellite directory is a miss, not a crash`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] (Some "fr") testAssemblySimpleName
                |> Option.map (fun assy -> assy.Name.Name)
                |> shouldEqual None
            )

    [<Test>]
    let ``a file whose name matches only ignoring case is found`` () =
        withRuntimeDir
            (testAssemblySimpleName.ToLowerInvariant () + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] None testAssemblySimpleName
                |> Option.map (fun assy -> assy.Name.Name)
                |> shouldEqual (Some testAssemblySimpleName)
            )

    [<Test>]
    let ``an upper-case extension is found on a case-sensitive host`` () =
        withRuntimeDir
            (testAssemblySimpleName.ToUpperInvariant () + ".DLL")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] None testAssemblySimpleName
                |> Option.map (fun assy -> assy.Name.Name)
                |> shouldEqual (Some testAssemblySimpleName)
            )

    /// `TestCandidateRefMatchesDef`: the file was chosen by its name, and the manifest inside it
    /// must agree before it answers.
    [<Test>]
    let ``a file whose manifest names another assembly does not bind`` () =
        withRuntimeDir
            "Alias.dll"
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request : AssemblyLoadRequest =
                    {
                        SimpleName = "Alias"
                        Version = requested unspecified unspecified unspecified unspecified
                        Culture = None
                        PublicKeyToken = None
                        Flags = 0
                    }

                match
                    snd (
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            request
                            LoadedAssemblies.empty
                            AssemblyBindCache.empty
                    )
                with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound (_, bound) -> failwith $"expected NotFound, bound %s{bound.Name.FullName}"
            )

    /// Measured on .NET 10 against an IL-only assembly: `MSIL`, `IA64` and `ARM` bind, `x86` and
    /// `AMD64` do not.
    [<Test>]
    let ``a requested processor architecture is compared with the image's`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request (flags : int) : AssemblyLoadRequest =
                    {
                        SimpleName = testAssemblySimpleName
                        Version = requested unspecified unspecified unspecified unspecified
                        Culture = None
                        PublicKeyToken = None
                        Flags = flags
                    }

                for flags in [ 0x20 ; 0x40 ] do
                    match
                        snd (
                            AssemblyBinding.tryBind
                                loggerFactory
                                [ root ]
                                (request flags)
                                LoadedAssemblies.empty
                                AssemblyBindCache.empty
                        )
                    with
                    | AssemblyBindResult.NotFound -> ()
                    | AssemblyBindResult.Bound _ -> failwith $"expected NotFound for flags 0x%x{flags}"

                // IA64 (0x30) and ARM (0x50) read as MSIL, the way CoreCLR bit-tests the field.
                for flags in [ 0 ; 0x10 ; 0x30 ; 0x50 ] do
                    match
                        snd (
                            AssemblyBinding.tryBind
                                loggerFactory
                                [ root ]
                                (request flags)
                                LoadedAssemblies.empty
                                AssemblyBindCache.empty
                        )
                    with
                    | AssemblyBindResult.Bound _ -> ()
                    | AssemblyBindResult.NotFound -> failwith $"expected Bound for flags 0x%x{flags}"
            )

    [<Test>]
    let ``binding registers the assembly once and answers the same instance thereafter`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request (major : uint16) : AssemblyLoadRequest =
                    {
                        SimpleName = testAssemblySimpleName.ToUpperInvariant ()
                        Version = requested major unspecified unspecified unspecified
                        Culture = None
                        PublicKeyToken = None
                        Flags = 0
                    }

                // Too high a version: not found, and nothing registered.
                match
                    snd (
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            (request 999us)
                            LoadedAssemblies.empty
                            AssemblyBindCache.empty
                    )
                with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound _ -> failwith "expected NotFound"

                let assemblies, first =
                    match
                        snd (
                            AssemblyBinding.tryBind
                                loggerFactory
                                [ root ]
                                (request unspecified)
                                LoadedAssemblies.empty
                                AssemblyBindCache.empty
                        )
                    with
                    | AssemblyBindResult.Bound (assemblies, assy) -> assemblies, assy
                    | AssemblyBindResult.NotFound -> failwith "expected Bound"

                assemblies.DefinitionNamesInLoadOrder
                |> Seq.toList
                |> shouldEqual [ first.Name.FullName ]

                // The second bind is answered from the context, not the disk: the directory is gone.
                Directory.Delete (root, true)
                Directory.CreateDirectory root |> ignore<DirectoryInfo>

                match
                    snd (
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            (request unspecified)
                            assemblies
                            AssemblyBindCache.empty
                    )
                with
                | AssemblyBindResult.Bound (again, second) ->
                    Object.ReferenceEquals (first, second) |> shouldEqual true

                    again.DefinitionNamesInLoadOrder
                    |> Seq.toList
                    |> shouldEqual [ first.Name.FullName ]
                | AssemblyBindResult.NotFound -> failwith "expected Bound from the load context"

                // A version the loaded one cannot satisfy is a miss even though it is loaded.
                match
                    snd (
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            (request 999us)
                            assemblies
                            AssemblyBindCache.empty
                    )
                with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound _ -> failwith "expected NotFound against the loaded version"
            )

    /// The sequences measured on .NET 10 that pin the failure cache's key and its ordering
    /// against the success cache; `sourcesPure/AssemblyLoadFailureCache.cs` runs the same
    /// sequences against the real runtime.
    [<Test>]
    let ``the failure cache is keyed by name, version and culture, and consulted after the success cache`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                let request
                    (name : string)
                    (major : uint16)
                    (culture : string option)
                    (flags : int)
                    : AssemblyLoadRequest
                    =
                    {
                        SimpleName = name
                        Version = requested major unspecified unspecified unspecified
                        Culture = culture
                        PublicKeyToken = None
                        Flags = flags
                    }

                let plain = request testAssemblySimpleName unspecified None 0
                let x86 = request (testAssemblySimpleName.ToLowerInvariant ()) unspecified None 0x20
                let msil = request testAssemblySimpleName unspecified None 0x10
                let tooHigh = request testAssemblySimpleName 999us None 0
                let french = request testAssemblySimpleName unspecified (Some "fr") 0

                let bind (cache : AssemblyBindCache) (assemblies : LoadedAssemblies) (r : AssemblyLoadRequest) =
                    let cache, result =
                        AssemblyBinding.tryBind loggerFactory [ root ] r assemblies cache

                    match result with
                    | AssemblyBindResult.Bound (assemblies, _) -> cache, assemblies, true
                    | AssemblyBindResult.NotFound -> cache, assemblies, false

                // An architecture miss, even under another spelling of the name, poisons the plain
                // request and the MSIL one.
                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty x86

                bound |> shouldEqual false
                let cache, assemblies, bound = bind cache assemblies plain
                bound |> shouldEqual false
                let cache, assemblies, bound = bind cache assemblies msil
                bound |> shouldEqual false
                // A version-specific request has its own key and still binds.
                let ownMajor =
                    (Assembly.readFile loggerFactory (Path.Combine (root, testAssemblySimpleName + ".dll")))
                        .Name.Version.Major
                    |> uint16

                let _, _, bound =
                    bind cache assemblies (request testAssemblySimpleName ownMajor None 0)

                bound |> shouldEqual true

                // A version miss and a culture miss leave the plain request alone.
                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty tooHigh

                bound |> shouldEqual false
                let cache, assemblies, bound = bind cache assemblies french
                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies plain
                bound |> shouldEqual true

                // A plain request that bound survives a later architecture miss for the same name.
                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty plain

                bound |> shouldEqual true
                let cache, assemblies, bound = bind cache assemblies x86
                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies plain
                bound |> shouldEqual true
            )

    let private plainRequest (name : string) : AssemblyLoadRequest =
        {
            SimpleName = name
            Version = requested unspecified unspecified unspecified unspecified
            Culture = None
            PublicKeyToken = None
            Flags = 0
        }

    let private bindsWith
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (root : string)
        (cache : AssemblyBindCache)
        (assemblies : LoadedAssemblies)
        (r : AssemblyLoadRequest)
        : AssemblyBindCache * LoadedAssemblies * bool
        =
        let cache, result =
            AssemblyBinding.tryBind loggerFactory [ root ] r assemblies cache

        match result with
        | AssemblyBindResult.Bound (assemblies, _) -> cache, assemblies, true
        | AssemblyBindResult.NotFound -> cache, assemblies, false

    /// Measured on .NET 10: a miss under a token poisons only requests under that token, and
    /// a version whose major is unspecified is no version at all in the key.
    [<Test>]
    let ``the failure key keeps the token and drops an unspecified version`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()
                let bind = bindsWith loggerFactory root
                let token = Array.zeroCreate<byte> 8

                let x86WithToken =
                    { plainRequest testAssemblySimpleName with
                        PublicKeyToken = Some token
                        Flags = 0x20
                    }

                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty x86WithToken

                bound |> shouldEqual false

                let cache, assemblies, bound =
                    bind cache assemblies (plainRequest testAssemblySimpleName)

                bound |> shouldEqual true

                let _, _, bound =
                    bind
                        cache
                        assemblies
                        { plainRequest testAssemblySimpleName with
                            PublicKeyToken = Some token
                        }

                bound |> shouldEqual false

                let unspecifiedMajor =
                    { plainRequest testAssemblySimpleName with
                        Version = requested unspecified 1us 2us 3us
                        Flags = 0x20
                    }

                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty unspecifiedMajor

                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies (plainRequest testAssemblySimpleName)
                bound |> shouldEqual false
            )

    /// Measured on .NET 10: the successes are remembered under the request's own spelling,
    /// the misses under any spelling.
    [<Test>]
    let ``the success cache is spelled case-sensitively`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()
                let bind = bindsWith loggerFactory root
                let lower = testAssemblySimpleName.ToLowerInvariant ()
                let upper = testAssemblySimpleName.ToUpperInvariant ()

                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty (plainRequest testAssemblySimpleName)

                bound |> shouldEqual true

                let cache, assemblies, bound =
                    bind
                        cache
                        assemblies
                        { plainRequest lower with
                            Flags = 0x20
                        }

                bound |> shouldEqual false

                let cache, assemblies, bound =
                    bind cache assemblies (plainRequest testAssemblySimpleName)

                bound |> shouldEqual true
                let cache, assemblies, bound = bind cache assemblies (plainRequest upper)
                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies (plainRequest lower)
                bound |> shouldEqual false
            )

    /// Measured on .NET 10: `/tmp/target`, `../target` and `a/b` are each reported not found.
    /// The file here exists at exactly the path the rooted name would reach, so a probe that
    /// followed it would bind.
    [<Test>]
    let ``a name with a separator, or a rooted one, is a miss without a probe`` () =
        withRuntimeDir
            "target.dll"
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()

                for name in [ Path.Combine (root, "target") ; "../target" ; "a/b" ; "sub\\target" ] do
                    match
                        snd (
                            AssemblyBinding.tryBind
                                loggerFactory
                                [ root ]
                                (plainRequest name)
                                LoadedAssemblies.empty
                                AssemblyBindCache.empty
                        )
                    with
                    | AssemblyBindResult.NotFound -> ()
                    | AssemblyBindResult.Bound (_, bound) -> failwith $"'%s{name}' bound %s{bound.Name.FullName}"

                match
                    snd (
                        AssemblyBinding.tryBind
                            loggerFactory
                            [ root ]
                            { plainRequest "target" with
                                Culture = Some "../fr"
                            }
                            LoadedAssemblies.empty
                            AssemblyBindCache.empty
                    )
                with
                | AssemblyBindResult.NotFound -> ()
                | AssemblyBindResult.Bound (_, bound) -> failwith $"culture '../fr' bound %s{bound.Name.FullName}"
            )

    /// Measured on .NET 10: after `Version=10.0` bound and an architecture miss poisoned the
    /// name, `Version=10.0.65535.3` still answers, and the unversioned request answers
    /// `65535.1.2.3`.
    [<Test>]
    let ``a bound spec's version is compared no further than the first unspecified component`` () =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()
                let bind = bindsWith loggerFactory root

                let own =
                    (Assembly.readFile loggerFactory (Path.Combine (root, testAssemblySimpleName + ".dll")))
                        .Name.Version

                let withVersion (v : RequestedAssemblyVersion) =
                    { plainRequest testAssemblySimpleName with
                        Version = v
                    }

                let majorMinor =
                    requested (uint16 own.Major) (uint16 own.Minor) unspecified unspecified

                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty (withVersion majorMinor)

                bound |> shouldEqual true

                let cache, assemblies, bound =
                    bind
                        cache
                        assemblies
                        { withVersion majorMinor with
                            Flags = 0x20
                        }

                bound |> shouldEqual false

                let _, _, bound =
                    bind
                        cache
                        assemblies
                        (withVersion (requested (uint16 own.Major) (uint16 own.Minor) unspecified 3us))

                bound |> shouldEqual true

                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty (plainRequest testAssemblySimpleName)

                bound |> shouldEqual true

                let cache, assemblies, bound =
                    bind
                        cache
                        assemblies
                        { plainRequest testAssemblySimpleName with
                            Flags = 0x20
                        }

                bound |> shouldEqual false

                let _, _, bound =
                    bind cache assemblies (withVersion (requested unspecified 1us 2us 3us))

                bound |> shouldEqual true
            )

    /// The request that spells an assembly's full identity, as `AssemblyName.FullName` would.
    let private fullIdentityRequest (assy : DumpedAssembly) : AssemblyLoadRequest =
        let v = assy.Name.Version

        {
            SimpleName = assy.Name.Name
            Version = requested (uint16 v.Major) (uint16 v.Minor) (uint16 v.Build) (uint16 v.Revision)
            Culture = Some ""
            PublicKeyToken =
                match assy.Name.GetPublicKeyToken () with
                | null -> None
                | token when token.Length = 0 -> None
                | token -> Some token
            Flags = int assy.Name.Flags
        }

    /// Measured on .NET 10: the entry assembly's own identity is a spec the runtime recorded when
    /// it loaded it, so an architecture miss under that identity does not cost it; an assembly a
    /// request read from disk has only the request's spec recorded, so the same miss under its
    /// identity does.
    [<Test>]
    let ``an assembly loaded by the host answers to its own identity after a miss under it; one loaded by name does not``
        ()
        =
        withRuntimeDir
            (testAssemblySimpleName + ".dll")
            (fun root ->
                let _messages, loggerFactory = LoggerFactory.makeTest ()
                let bind = bindsWith loggerFactory root

                let image =
                    Assembly.readFile loggerFactory (Path.Combine (root, testAssemblySimpleName + ".dll"))

                let full = fullIdentityRequest image

                let fullX86 =
                    { full with
                        Flags = full.Flags ||| 0x20
                    }

                // Loaded by the host: the identity is recorded.
                let hosted = LoadedAssemblies.ofAssemblies [ image ]
                let cache, assemblies, bound = bind AssemblyBindCache.empty hosted fullX86
                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies full
                bound |> shouldEqual true

                // Loaded by a plain request: only that request's spec is recorded.
                let cache, assemblies, bound =
                    bind AssemblyBindCache.empty LoadedAssemblies.empty (plainRequest testAssemblySimpleName)

                bound |> shouldEqual true
                let cache, assemblies, bound = bind cache assemblies fullX86
                bound |> shouldEqual false
                let _, _, bound = bind cache assemblies full
                bound |> shouldEqual false
            )

    /// `IsValidArchitecture` is asked before any candidate is read: a request for x86 is a miss
    /// on every platform PawPrint simulates without the file being opened, which a file that is
    /// not an image at all makes visible.
    [<Test>]
    let ``an x86 request misses before any file is read`` () =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (root, "Broken.dll"), [| 0uy ; 1uy ; 2uy ; 3uy |])
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            let request =
                { plainRequest "Broken" with
                    Flags = 0x20
                }

            match
                snd (
                    AssemblyBinding.tryBind
                        loggerFactory
                        [ root ]
                        request
                        LoadedAssemblies.empty
                        AssemblyBindCache.empty
                )
            with
            | AssemblyBindResult.NotFound -> ()
            | AssemblyBindResult.Bound (_, bound) -> failwith $"bound %s{bound.Name.FullName}"
        finally
            Directory.Delete (root, true)

    /// Two files differing only by case are a collision however the request spells the name,
    /// so the answer cannot depend on the request's casing. Only a case-sensitive filesystem
    /// can hold the pair.
    [<Test>]
    let ``files differing only by case are refused whatever the request's casing`` () =
        let root =
            Path.Combine (Path.GetTempPath (), "PawPrint-" + Guid.NewGuid().ToString "N")

        Directory.CreateDirectory root |> ignore<DirectoryInfo>

        try
            let image = typeof<BindingTestMarker>.Assembly.Location
            File.Copy (image, Path.Combine (root, testAssemblySimpleName + ".dll"))
            let lower = Path.Combine (root, testAssemblySimpleName.ToLowerInvariant () + ".dll")

            if File.Exists lower then
                Assert.Ignore "the filesystem is case-insensitive, so the pair cannot exist"

            File.Copy (image, lower)
            let _messages, loggerFactory = LoggerFactory.makeTest ()

            for name in [ testAssemblySimpleName ; testAssemblySimpleName.ToLowerInvariant () ] do
                let refused =
                    try
                        AssemblyBinding.tryReadFromRuntimeDirs loggerFactory [ root ] None name
                        |> ignore<DumpedAssembly option>

                        false
                    with e when e.Message.Contains "differing only by case" ->
                        true

                refused |> shouldEqual true
        finally
            Directory.Delete (root, true)
