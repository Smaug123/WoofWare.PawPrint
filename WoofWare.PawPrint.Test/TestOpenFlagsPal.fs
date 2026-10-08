namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `OpenFlagsPal` transcribes an upstream enum, the screen the shim applies over
/// it, and each platform's `<fcntl.h>` numbering, so nothing in the type system
/// keeps its numbers right. Its oracles: the PAL values re-read from the pinned
/// `pal_io.h`, and each platform's numbering read from the headers the kernel
/// library's `open-flags.c` probe printed on that platform.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpenFlagsPal =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    // ------------------------------------------------------------ the PAL values

    let private requireRuntimeSrc () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir -> dir

    [<Test>]
    let ``every PAL value is the one the pinned header defines`` () : unit =
        let path =
            Path.Combine (requireRuntimeSrc (), "src", "native", "libs", "System.Native", "pal_io.h")

        let pinned =
            Regex.Matches (
                File.ReadAllText path,
                @"^\s+PAL_O_(?<name>[A-Z_]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),",
                RegexOptions.Multiline
            )
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        pinned
        |> shouldEqual (
            Map.ofList
                [
                    "RDONLY", OpenFlagsPal.ReadOnly
                    "WRONLY", OpenFlagsPal.WriteOnly
                    "RDWR", OpenFlagsPal.ReadWrite
                    "ACCESS_MODE_MASK", OpenFlagsPal.AccessModeMask
                    "CLOEXEC", OpenFlagsPal.CloseOnExec
                    "CREAT", OpenFlagsPal.Create
                    "EXCL", OpenFlagsPal.Exclusive
                    "TRUNC", OpenFlagsPal.Truncate
                    "SYNC", OpenFlagsPal.Synchronous
                    "NOFOLLOW", OpenFlagsPal.NoFollow
                ]
        )

    // -------------------------------------------------------- the platform numbers

    /// The `HEADER` lines of the probe's output on `platform`'s kernel.
    let private headers (platform : SimulatedUnixPlatform) : Map<string, int> =
        let leaf =
            match SimulatedUnixPlatform.flavour platform, SimulatedUnixPlatform.architecture platform with
            | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64 -> "linuxX64"
            | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64 -> "linuxArm64"
            | SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64 -> "darwin"
            | flavour, architecture -> failwith $"no probe output for %O{flavour} on %O{architecture}"

        let resource = $"WoofWare.PawPrint.Test.openFlags.%s{leaf}.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.choose (fun line ->
            match line.Split '\t' with
            | [| "HEADER" ; name ; value |] -> Some (name, Convert.ToUInt32 (value, 16) |> int)
            | _ -> None
        )
        |> Map.ofArray

    /// What `ConvertOpenFlags` hands `open(2)` on the platform whose probe
    /// printed `header`, transcribed from the C: the access mode under its
    /// four-bit mask first, then any bit it does not know, each -1; otherwise
    /// each PAL bit's `<fcntl.h>` bit.
    let private convertOpenFlags (header : Map<string, int>) (flags : int) : int option =

        let access =
            match flags &&& 0xF with
            | 0 -> Some header.["O_RDONLY"]
            | 1 -> Some header.["O_WRONLY"]
            | 2 -> Some header.["O_RDWR"]
            | _ -> None

        match access with
        | None -> None
        | Some _ when flags &&& ~~~0x3FF <> 0 -> None
        | Some access ->
            [
                0x10, "O_CLOEXEC"
                0x20, "O_CREAT"
                0x40, "O_EXCL"
                0x80, "O_TRUNC"
                0x100, "O_SYNC"
                0x200, "O_NOFOLLOW"
            ]
            |> List.fold (fun word (pal, name) -> if flags &&& pal <> 0 then word ||| header.[name] else word) access
            |> Some

    [<Test>]
    let ``every PAL word up to 0x10000 converts as the shim converts it`` () : unit =
        for platform in platforms do
            let header = headers platform

            for flags in 0..0xFFFF do
                (flags, OpenFlagsPal.decode platform flags)
                |> shouldEqual (flags, convertOpenFlags header flags)

    [<Test>]
    let ``every PAL word converts as the shim converts it`` () : unit =
        for platform in platforms do
            let header = headers platform

            let property =
                Prop.forAll
                    (Arb.fromGen (Gen.choose (Int32.MinValue, Int32.MaxValue)))
                    (fun flags ->
                        OpenFlagsPal.decode platform flags = convertOpenFlags header flags
                        |> Prop.label $"%O{platform} 0x%x{flags}"
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 10000, property)

    [<Test>]
    let ``opendir's word is O_RDONLY|O_DIRECTORY|O_CLOEXEC`` () : unit =
        for platform in platforms do
            let header = headers platform

            OpenFlagsPal.directoryStream platform
            |> shouldEqual (header.["O_RDONLY"] ||| header.["O_DIRECTORY"] ||| header.["O_CLOEXEC"])

    // ------------------------------------------------- what the kernel makes of it

    /// Every word the shim can hand the kernel is one the kernel library
    /// answers: a guest calling `SystemNative_Open` meets no refusal for its
    /// flags alone.
    [<Test>]
    let ``the kernel answers every word the shim can pass`` () : unit =
        for platform in platforms do
            let system : UnixSystem<int, string> =
                UnixSystem.initial platform
                |> (Launched.bootWith
                        (Launched.credentials (
                            Credentials.ofIds
                                (UserId.parseOrFail "TestOpenFlagsPal" 1000u)
                                (GroupId.parseOrFail "TestOpenFlagsPal" 1000u)
                                []
                        ))
                        UnixSystem.pipedStandardStreams
                        0
                        (CpuId 0))

            let words =
                [
                    yield OpenFlagsPal.directoryStream platform
                    for flags in 0..0x3FF do
                        match OpenFlagsPal.decode platform flags with
                        | Some word -> yield word
                        | None -> ()
                ]

            // Three access modes and the six bits.
            words.Length |> shouldEqual (1 + 3 * 64)

            for word in words do
                match
                    UnixNamespace.openPath
                        word
                        (PathArgumentBytes.Bytes (UnixPath.toByteString (UnixPath.parseOrFail "test" "/")))
                        0o644
                        system
                with
                | Ok _ -> ()
                | Error refusal -> failwith $"%O{platform} 0x%x{word}: %s{OpenRefusal.describe refusal}"
