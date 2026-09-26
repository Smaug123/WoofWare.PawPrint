namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `UnixErrorPal.toPal` is a second exhaustive match over `UnixError`, so the
/// compiler keeps it complete but cannot keep it correct. Its oracle is upstream
/// rather than the library: every value is re-derived here from the pinned
/// `Interop.Errors.cs`, which is the same authority the joint table had before
/// the two columns were split.
///
/// That is what makes the mirror safe. Nothing checks the PAL column against
/// `WoofWare.PosixKernel`, and nothing should — the library states raw errnos
/// and has no opinion about .NET's numbering.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixErrorPal =

    let private runtimeSrc : string option =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private requireRuntimeSrc () : string =
        match runtimeSrc with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"

    let private caseName (error : UnixError) : string = sprintf "%O" error

    // ---------------------------------------------------------------------
    // The PAL column, against the pinned `Interop.Errors.cs`.
    // ---------------------------------------------------------------------

    /// `EPERM = 0x10042,   // Operation not permitted.` and friends.
    let private palEntry : Regex =
        Regex (@"^\s+(?<name>E[A-Z0-9]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),", RegexOptions.Multiline)

    /// `EOPNOTSUPP      = ENOTSUP,` and friends: the members the enum defines by
    /// naming another member rather than a literal. They are as real as the rest
    /// — CoreLib switches on the value either way — so a `UnixError` named after
    /// one still has an oracle, and resolving them here is what lets it.
    let private palAlias : Regex =
        Regex (@"^\s+(?<name>E[A-Z0-9]+)\s*=\s*(?<target>E[A-Z0-9]+),", RegexOptions.Multiline)

    let private pinnedPalValues () : Map<string, int> =
        let path =
            Path.Combine (
                requireRuntimeSrc (),
                "src",
                "libraries",
                "Common",
                "src",
                "Interop",
                "Unix",
                "Interop.Errors.cs"
            )

        if not (File.Exists path) then
            failwith
                $"TestUnixErrorPal: expected the pinned PAL error enum at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common/src/Interop/Unix, the PAL table has lost its oracle."

        let text = File.ReadAllText path

        let literals =
            palEntry.Matches text
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        // One pass is enough: the enum defines no alias of an alias, and an
        // unresolvable target would mean the file changed shape, so it fails
        // loudly rather than quietly dropping the member.
        palAlias.Matches text
        |> Seq.fold
            (fun (acc : Map<string, int>) m ->
                let name = m.Groups.["name"].Value
                let target = m.Groups.["target"].Value

                match Map.tryFind target literals with
                | Some value -> Map.add name value acc
                | None ->
                    failwith
                        $"TestUnixErrorPal: the pinned enum aliases %s{name} to %s{target}, which has no literal value. The enum's shape has changed; teach this test to resolve it."
            )
            literals

    /// The `Interop.Error` members that are no errno: success, the two
    /// failures the shim synthesises beyond the kernel, and `EWOULDBLOCK`, the
    /// enum's second spelling of `EAGAIN` (which `UnixError` spells once).
    let private notAnErrno : Set<string> =
        Set.ofList [ "SUCCESS" ; "EHOSTNOTFOUND" ; "ESOCKETERROR" ; "EWOULDBLOCK" ]

    /// A case the enum names has that value. Any other case is an errno the
    /// shim's `ConvertErrorPlatformToPal` has no `case` for, so it falls through
    /// to `ENONSTANDARD`.
    [<Test>]
    let ``PAL values agree with the pinned Interop.Errors.cs`` () : unit =
        let pinned = pinnedPalValues ()

        // Guard against the regex silently matching nothing and the test then
        // passing vacuously.
        pinned |> Map.count |> shouldBeGreaterThan 50

        let mutable unnamed = 0

        for error in UnixError.all do
            match Map.tryFind (caseName error) pinned with
            | Some expected -> UnixErrorPal.toPal error |> shouldEqual expected
            | None ->
                unnamed <- unnamed + 1
                UnixErrorPal.toPal error |> shouldEqual UnixErrorPal.palNonStandard

        // Every one-flavour error (Linux's forty-four, Darwin's nineteen) and
        // seven both have: `ENOTBLK`, `EREMOTE`, `EUSERS`, `ETOOMANYREFS` and
        // the STREAMS trio `ENOSTR`, `ENOSR`, `ETIME`. Counted, so neither arm
        // is vacuous.
        unnamed |> shouldEqual 70

    /// The converse: `toPal` can produce every errno the enum names. A member
    /// with no case would be a PAL value no conversion here can reach.
    [<Test>]
    let ``every Interop.Error errno is some case's PAL value`` () : unit =
        let cases = UnixError.all |> List.map caseName |> Set.ofList

        let missing =
            pinnedPalValues ()
            |> Map.keys
            |> Seq.filter (fun name -> not (Set.contains name notAnErrno) && not (Set.contains name cases))
            |> Seq.toList

        missing |> shouldEqual []

    [<Test>]
    let ``PAL numbering is injective, up to the enum's own alias and ENONSTANDARD`` () : unit =
        // `Interop.Error` defines `EOPNOTSUPP = ENOTSUP`, so those two are one
        // value by the enum's own declaration, and every case the enum does
        // not name shares `ENONSTANDARD`; any other collision is ours.
        let collisions =
            UnixError.all
            |> List.groupBy UnixErrorPal.toPal
            |> List.filter (fun (pal, errors) -> List.length errors > 1 && pal <> UnixErrorPal.palNonStandard)
            |> List.filter (fun (_, errors) ->
                Set.ofList errors <> Set.ofList [ UnixError.EOPNOTSUPP ; UnixError.ENOTSUP ]
            )

        collisions |> shouldEqual []

    [<Test>]
    let ``a platform-dependent error still has a usable PAL value`` () : unit =
        // The whole point of `UnixError` admitting ELOOP: CoreLib switches on
        // the PAL value, which is answerable, while its raw number is not --
        // `TestUnixError` asserts that half, in the library's own fixture.
        UnixErrorPal.toPal UnixError.ELOOP |> shouldEqual 0x10020

    [<Test>]
    let ``ofRawErrno maps zero to SUCCESS`` () : unit =
        UnixErrorPal.ofRawErrno 0 |> shouldEqual UnixErrorPal.palSuccess

    [<Test>]
    let ``ofRawErrno inverts toRawErrno wherever toRawErrno answers`` () : unit =
        for error in UnixError.all do
            match UnixError.rawNumbering error with
            | RawErrnoPortability.PlatformDependent _
            | RawErrnoPortability.LinuxOnly _
            | RawErrnoPortability.DarwinOnly _ ->
                // Not invertible, and deliberately so: see the refusal test
                // below, which drives both of ELOOP's candidate numbers.
                ()
            | RawErrnoPortability.Portable _ ->
                UnixErrorPal.ofRawErrno (UnixError.toRawErrno error)
                |> shouldEqual (UnixErrorPal.toPal error)

    /// ENOTBLK is 15 on both Linux and Darwin, so its meaning needs no platform
    /// choice — but `Interop.Error` has no entry for it, so upstream's switch
    /// falls through to ENONSTANDARD. We must do the same rather than crash:
    /// this conversion is unambiguous, it just has no PAL name. Today this is
    /// the only raw errno in that class.
    [<Test>]
    let ``ofRawErrno reports ENONSTANDARD for a portable errno with no PAL name`` () : unit =
        UnixErrorPal.ofRawErrno 15 |> shouldEqual UnixErrorPal.palNonStandard

    /// POSIX requires errno values to be positive, so a negative number names an
    /// error on no Unix we model and every platform's switch falls through to
    /// ENONSTANDARD. Answering that needs no platform choice, so it must not
    /// crash. -0x20001 and -0x20002 are upstream's synthetic EHOSTNOTFOUND and
    /// ESOCKETERROR, which is how a negative most plausibly reaches here.
    [<TestCase -1>]
    [<TestCase -34>]
    [<TestCase 0x80000000>]
    [<TestCase -0x20001>]
    [<TestCase -0x20002>]
    let ``ofRawErrno reports ENONSTANDARD for a negative errno`` (raw : int) : unit =
        UnixErrorPal.ofRawErrno raw |> shouldEqual UnixErrorPal.palNonStandard

    /// An errno whose meaning depends on the platform
    /// must not be silently resolved. 11 and 35 are the transposed
    /// EAGAIN/EDEADLK pair; 39 is ENOTEMPTY on Linux and EDESTADDRREQ on Darwin;
    /// 40 is ELOOP on Linux and EMSGSIZE on Darwin.
    [<TestCase 11>]
    [<TestCase 35>]
    [<TestCase 39>]
    [<TestCase 40>]
    [<TestCase 62>]
    [<TestCase 66>]
    let ``ofRawErrno refuses a platform-dependent errno`` (raw : int) : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> UnixErrorPal.ofRawErrno raw |> ignore<int>)

        exn.Message |> shouldContainText "platform-dependent"

    /// The same numbers, answered rather than refused, once the caller has said
    /// which Unix it impersonates. This is the entry point every handler inside
    /// the emulated kernel uses, and nothing pinned it before.
    [<Test>]
    let ``ofRawErrnoUnder resolves what ofRawErrno refuses`` () : unit =
        UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Linux 40
        |> shouldEqual (UnixErrorPal.toPal UnixError.ELOOP)

        UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Darwin 62
        |> shouldEqual (UnixErrorPal.toPal UnixError.ELOOP)

        // And the two numberings genuinely disagree on the same input: raw 40
        // under Darwin is EMSGSIZE, not ELOOP.
        UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Darwin 40
        |> shouldEqual (UnixErrorPal.toPal UnixError.EMSGSIZE)

        // A number that is a different error on each platform, and is named
        // by the enum on one only: Linux's 72 is EMULTIHOP, Darwin's EBADRPC.
        UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Linux 72
        |> shouldEqual (UnixErrorPal.toPal UnixError.EMULTIHOP)

        UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Darwin 72
        |> shouldEqual UnixErrorPal.palNonStandard

    // ---------------------------------------------------------------------
    // ofRawErrnoUnder, against the shim itself.
    // ---------------------------------------------------------------------

    /// What the real shim's `SystemNative_ConvertErrorPlatformToPal` answered
    /// for every raw number in [-300, 4096] and at `Int32.MinValue`,
    /// `Int32.MaxValue`, -0x20001 and -0x20002, listing only the answers that
    /// were not `ENONSTANDARD`. Measured with
    /// `docs/plans/2026-08-23-posix-kernel-extraction/errno-table.sh` against
    /// the shim of .NET 10.0.11 (Linux aarch64) and 10.0.12 (Linux x86-64
    /// under Rosetta), which answered identically, and of 10.0.7 on Darwin
    /// 27.0.0 arm64.
    let private measuredShim (numbering : RawErrnoNumbering) : Map<int, int> =
        match numbering with
        | RawErrnoNumbering.Linux ->
            Map.ofList
                [
                    1, 0x10042
                    2, 0x1002D
                    3, 0x1004A
                    4, 0x1001B
                    5, 0x1001D
                    6, 0x1003F
                    7, 0x10001
                    8, 0x1002E
                    9, 0x10008
                    10, 0x1000C
                    11, 0x10006
                    12, 0x10031
                    13, 0x10002
                    14, 0x10015
                    16, 0x1000A
                    17, 0x10014
                    18, 0x1004F
                    19, 0x1002C
                    20, 0x10039
                    21, 0x1001F
                    22, 0x1001C
                    23, 0x10029
                    24, 0x10021
                    25, 0x1003E
                    26, 0x1004E
                    27, 0x10016
                    28, 0x10034
                    29, 0x10049
                    30, 0x10048
                    31, 0x10022
                    32, 0x10043
                    33, 0x10012
                    34, 0x10047
                    35, 0x10010
                    36, 0x10025
                    37, 0x1002F
                    38, 0x10037
                    39, 0x1003A
                    40, 0x10020
                    42, 0x10032
                    43, 0x10018
                    61, 0x10071
                    67, 0x10030
                    71, 0x10044
                    72, 0x10024
                    74, 0x10009
                    75, 0x10040
                    84, 0x10019
                    88, 0x1003C
                    89, 0x10011
                    90, 0x10023
                    91, 0x10046
                    92, 0x10033
                    93, 0x10045
                    94, 0x1005E
                    95, 0x1003D
                    96, 0x10060
                    97, 0x10005
                    98, 0x10003
                    99, 0x10004
                    100, 0x10026
                    101, 0x10028
                    102, 0x10027
                    103, 0x1000D
                    104, 0x1000F
                    105, 0x1002A
                    106, 0x1001E
                    107, 0x10038
                    108, 0x1006C
                    110, 0x1004D
                    111, 0x1000E
                    112, 0x10070
                    113, 0x10017
                    114, 0x10007
                    115, 0x1001A
                    116, 0x1004B
                    122, 0x10013
                    125, 0x1000B
                    130, 0x10041
                    131, 0x1003B
                ]
        | RawErrnoNumbering.Darwin ->
            Map.ofList
                [
                    1, 0x10042
                    2, 0x1002D
                    3, 0x1004A
                    4, 0x1001B
                    5, 0x1001D
                    6, 0x1003F
                    7, 0x10001
                    8, 0x1002E
                    9, 0x10008
                    10, 0x1000C
                    11, 0x10010
                    12, 0x10031
                    13, 0x10002
                    14, 0x10015
                    16, 0x1000A
                    17, 0x10014
                    18, 0x1004F
                    19, 0x1002C
                    20, 0x10039
                    21, 0x1001F
                    22, 0x1001C
                    23, 0x10029
                    24, 0x10021
                    25, 0x1003E
                    26, 0x1004E
                    27, 0x10016
                    28, 0x10034
                    29, 0x10049
                    30, 0x10048
                    31, 0x10022
                    32, 0x10043
                    33, 0x10012
                    34, 0x10047
                    35, 0x10006
                    36, 0x1001A
                    37, 0x10007
                    38, 0x1003C
                    39, 0x10011
                    40, 0x10023
                    41, 0x10046
                    42, 0x10033
                    43, 0x10045
                    44, 0x1005E
                    45, 0x1003D
                    46, 0x10060
                    47, 0x10005
                    48, 0x10003
                    49, 0x10004
                    50, 0x10026
                    51, 0x10028
                    52, 0x10027
                    53, 0x1000D
                    54, 0x1000F
                    55, 0x1002A
                    56, 0x1001E
                    57, 0x10038
                    58, 0x1006C
                    60, 0x1004D
                    61, 0x1000E
                    62, 0x10020
                    63, 0x10025
                    64, 0x10070
                    65, 0x10017
                    66, 0x1003A
                    69, 0x10013
                    70, 0x1004B
                    77, 0x1002F
                    78, 0x10037
                    84, 0x10040
                    89, 0x1000B
                    90, 0x10018
                    91, 0x10032
                    92, 0x10019
                    94, 0x10009
                    95, 0x10024
                    96, 0x10071
                    97, 0x10030
                    100, 0x10044
                    102, 0x1003D
                    104, 0x1003B
                    105, 0x10041
                ]

    let private sweep : int list =
        [ Int32.MinValue ; -0x20002 ; -0x20001 ; Int32.MaxValue ] @ [ -300 .. 4096 ]

    [<Test>]
    let ``ofRawErrnoUnder answers what each flavour's shim was measured to`` () : unit =
        for numbering in [ RawErrnoNumbering.Linux ; RawErrnoNumbering.Darwin ] do
            let measured = measuredShim numbering

            for raw in sweep do
                let expected =
                    if raw = 0 then
                        UnixErrorPal.palSuccess
                    else
                        Map.tryFind raw measured |> Option.defaultValue UnixErrorPal.palNonStandard

                let actual = UnixErrorPal.ofRawErrnoUnder numbering raw

                if actual <> expected then
                    failwith
                        $"TestUnixErrorPal: under %O{numbering}, raw %d{raw} converts to 0x%X{actual}, but the shim was measured answering 0x%X{expected}."

    /// The shim this test host runs against. Pure: a switch over the
    /// `<errno.h>` constants it was compiled with.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_ConvertErrorPlatformToPal")>]
    extern int private hostConvertErrorPlatformToPal(int platformErrno)

    /// The measurement above, repeated against whatever shim this host has:
    /// the Darwin half on a dev box, the Linux half in CI.
    [<Test>]
    let ``ofRawErrnoUnder answers what this host's shim does`` () : unit =
        let numbering =
            if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
                Some RawErrnoNumbering.Darwin
            elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
                Some RawErrnoNumbering.Linux
            else
                None

        match numbering with
        | None -> Assert.Ignore $"no modelled Unix to measure (%s{RuntimeInformation.OSDescription})"
        | Some numbering ->

        for raw in sweep do
            let expected = hostConvertErrorPlatformToPal raw
            let actual = UnixErrorPal.ofRawErrnoUnder numbering raw

            if actual <> expected then
                failwith
                    $"TestUnixErrorPal: under %O{numbering}, raw %d{raw} converts to 0x%X{actual}, but this host's shim answers 0x%X{expected}."
