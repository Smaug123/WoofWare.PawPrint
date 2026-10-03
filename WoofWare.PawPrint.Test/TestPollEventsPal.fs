namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `PollEventsPal` transcribes two upstream functions, so nothing in the type
/// system keeps its numbers right. The PAL half of its oracle is upstream: the
/// six `PollEvents` values and each conversion's rows are re-derived here from
/// the pinned `pal_io_common.h`. The platform half is each flavour's
/// `<poll.h>`, stated below as measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPollEventsPal =

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

    let private pinnedSource () : string =
        let path =
            Path.Combine (requireRuntimeSrc (), "src", "native", "libs", "Common", "pal_io_common.h")

        if not (File.Exists path) then
            failwith
                $"TestPollEventsPal: expected the pinned PAL poll source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/Common, this transcription has lost its oracle."

        File.ReadAllText path

    /// `PAL_POLLIN = 0x0001,` and friends.
    let private palEntry : Regex =
        Regex (@"^\s+(?<name>PAL_POLL[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),", RegexOptions.Multiline)

    let private pinnedPollEvents () : Map<string, int16> =
        let values =
            palEntry.Matches (pinnedSource ())
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt16 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        if values.Count <> 6 then
            failwith
                $"TestPollEventsPal: read %d{values.Count} PollEvents values from the pinned pal_io_common.h, expected 6. The enum's shape has changed; teach this test to read it."

        values

    let private pinned (name : string) : int16 =
        match Map.tryFind name (pinnedPollEvents ()) with
        | Some value -> value
        | None ->
            failwith
                $"TestPollEventsPal: the pinned pal_io_common.h has no %s{name}. The enum has been renamed upstream."

    /// The six platform bits the conversions name, as both flavours' `<poll.h>`
    /// number them: measured 2026-09-23 on Linux 6.18.5 and Darwin 25.6.0 by
    /// `docs/plans/2026-08-23-posix-kernel-extraction/poll-alphabet.c`, which
    /// printed the same six values on both.
    let private platform : Map<string, int16> =
        Map.ofList
            [
                "POLLIN", 0x0001s
                "POLLPRI", 0x0002s
                "POLLOUT", 0x0004s
                "POLLERR", 0x0008s
                "POLLHUP", 0x0010s
                "POLLNVAL", 0x0020s
            ]

    // ---------------------------------------------------------------------
    // The alphabet itself.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``the six PollEvents values are upstream's`` () : unit =
        pinned "PAL_POLLIN" |> shouldEqual 0x0001s
        pinned "PAL_POLLPRI" |> shouldEqual 0x0002s
        pinned "PAL_POLLOUT" |> shouldEqual 0x0004s
        pinned "PAL_POLLERR" |> shouldEqual 0x0008s
        pinned "PAL_POLLHUP" |> shouldEqual 0x0010s
        pinned "PAL_POLLNVAL" |> shouldEqual 0x0020s

    // ---------------------------------------------------------------------
    // Which bit maps to which, read out of upstream's own function bodies.
    // The values above are only half an oracle: a pin that re-paired the rows
    // without renumbering them would leave a test that checked numbers alone
    // entirely green.
    // ---------------------------------------------------------------------

    /// The body of a function in a C file, from its signature to the closing
    /// brace in column 0.
    let private functionBody (signature : string) : string =
        let source = pinnedSource ()

        match source.IndexOf (signature, StringComparison.Ordinal) with
        | -1 ->
            failwith
                $"TestPollEventsPal: the pinned pal_io_common.h no longer declares `%s{signature}`. The conversion this transcribes has been renamed or resignatured upstream."
        | start ->

        let body = source.Substring start

        match body.IndexOf ("\n}", StringComparison.Ordinal) with
        | -1 -> failwith $"TestPollEventsPal: `%s{signature}` has no closing brace in column 0."
        | finish -> body.Substring (0, finish)

    /// `if ((palEvents & PAL_POLLIN) != 0) { platformEvents |= POLLIN; }` and
    /// friends: one row of a conversion, in whichever direction it runs.
    let private conversionRow : Regex =
        Regex (@"if\s*\(\(\w+\s*&\s*(?<from>\w+)\)\s*!=\s*0\)\s*\{\s*\w+\s*\|=\s*(?<to>\w+);")

    let private conversionRows (signature : string) : (string * string) list =
        let rows =
            conversionRow.Matches (functionBody signature)
            |> Seq.map (fun m -> m.Groups.["from"].Value, m.Groups.["to"].Value)
            |> List.ofSeq

        if rows.Length <> 6 then
            failwith
                $"TestPollEventsPal: read %d{rows.Length} conversion rows from `%s{signature}`, expected 6. The function's shape has changed; teach this test to read it."

        rows

    let private platformBit (name : string) : int16 =
        match Map.tryFind name platform with
        | Some bit -> bit
        | None ->
            failwith
                $"TestPollEventsPal: upstream's conversion names %s{name}, which is not one of the six platform bits measured above."

    [<Test>]
    let ``toPlatform pairs the bits as Common_ConvertPollEventsPalToPlatform does`` () : unit =
        let rows =
            conversionRows "inline static int16_t Common_ConvertPollEventsPalToPlatform(int16_t palEvents)"

        for pal, platformName in rows do
            PollEventsPal.toPlatform (pinned pal) |> shouldEqual (platformBit platformName)

        // Each row pairs a PAL name with the platform name it spells, and all
        // six PAL values appear: the table re-pairs nothing and misses nothing.
        rows
        |> List.map fst
        |> Set.ofList
        |> shouldEqual (pinnedPollEvents () |> Map.keys |> Set.ofSeq)

        for pal, platformName in rows do
            pal |> shouldEqual ("PAL_" + platformName)

    [<Test>]
    let ``ofPlatform pairs the bits as Common_ConvertPollEventsPlatformToPal does`` () : unit =
        let rows =
            conversionRows "inline static int16_t Common_ConvertPollEventsPlatformToPal(int16_t platformEvents)"

        for platformName, pal in rows do
            PollEventsPal.ofPlatform (platformBit platformName) |> shouldEqual (pinned pal)

        rows
        |> List.map snd
        |> Set.ofList
        |> shouldEqual (pinnedPollEvents () |> Map.keys |> Set.ofSeq)

    /// The rows above pin one bit at a time; this pins that each conversion is
    /// their union, and that every other bit is dropped rather than carried
    /// through -- over all 65536 inputs.
    [<Test>]
    let ``each conversion is the union of its rows and drops every other bit`` () : unit =
        let pairs =
            [
                "PAL_POLLIN", "POLLIN"
                "PAL_POLLPRI", "POLLPRI"
                "PAL_POLLOUT", "POLLOUT"
                "PAL_POLLERR", "POLLERR"
                "PAL_POLLHUP", "POLLHUP"
                "PAL_POLLNVAL", "POLLNVAL"
            ]
            |> List.map (fun (pal, platformName) -> pinned pal, platformBit platformName)

        let mismatches =
            [
                for raw in 0..0xFFFF do
                    let bits = int16 (uint16 raw)

                    let expectedPlatform =
                        pairs
                        |> List.fold
                            (fun acc (pal, platform) -> if bits &&& pal <> 0s then acc ||| platform else acc)
                            0s

                    let expectedPal =
                        pairs
                        |> List.fold
                            (fun acc (pal, platform) -> if bits &&& platform <> 0s then acc ||| pal else acc)
                            0s

                    if PollEventsPal.toPlatform bits <> expectedPlatform then
                        yield $"toPlatform 0x%04x{raw}"

                    if PollEventsPal.ofPlatform bits <> expectedPal then
                        yield $"ofPlatform 0x%04x{raw}"
            ]

        mismatches |> List.truncate 10 |> shouldEqual []

    // ---------------------------------------------------------------------
    // The composition `SystemNative_Poll` answers with.
    // ---------------------------------------------------------------------

    /// Every combination of the five conditions a descriptor's level states.
    let private everyLevel : ReadinessLevel list =
        [
            for bits in 0..31 ->
                {
                    In = bits &&& 0x01 <> 0
                    Out = bits &&& 0x02 <> 0
                    RdHup = bits &&& 0x04 <> 0
                    Hup = bits &&& 0x08 <> 0
                    Err = bits &&& 0x10 <> 0
                }
        ]

    /// `level` as Linux's `<poll.h>` numbers it, which below 0x10000 is
    /// `<sys/epoll.h>`'s numbering.
    let private linuxBits (level : ReadinessLevel) : int16 =
        (if level.In then EpollEvents.In else 0u)
        ||| (if level.Out then EpollEvents.Out else 0u)
        ||| (if level.RdHup then EpollEvents.RdHup else 0u)
        ||| (if level.Hup then EpollEvents.Hup else 0u)
        ||| (if level.Err then EpollEvents.Err else 0u)
        |> int16

    /// What `SystemNative_Poll` answered for one entry at `level` before the
    /// library spoke Linux's own alphabet: the six-bit projection of the level,
    /// as the guest-visible behaviour this composition must keep bit for bit.
    /// `IN` and `OUT` when asked for; `ERR` and `HUP` whatever was asked;
    /// `PRI` never.
    ///
    /// `pal` is the pinned `PollEvents` values, read once by the caller.
    let private sixBitProjection (pal : Map<string, int16>) (level : ReadinessLevel) (palEvents : int16) : int16 =
        (if level.In && palEvents &&& pal.["PAL_POLLIN"] <> 0s then
             pal.["PAL_POLLIN"]
         else
             0s)
        ||| (if level.Out && palEvents &&& pal.["PAL_POLLOUT"] <> 0s then
                 pal.["PAL_POLLOUT"]
             else
                 0s)
        ||| (if level.Err then pal.["PAL_POLLERR"] else 0s)
        ||| (if level.Hup then pal.["PAL_POLLHUP"] else 0s)

    /// Every PAL request mask, all 65536 of them, at every level: converted in,
    /// answered as Linux's `do_pollfd` answers (the level restricted to what
    /// was asked, plus `POLLERR` and `POLLHUP`; see `LinuxReadiness`), and
    /// converted out, each entry reports exactly what the six-bit projection
    /// reported. An entry reports something to the PAL exactly when it reports
    /// something to the kernel's caller, so `poll(2)`'s count, which the shim
    /// passes on unconverted, counts the PAL's reports.
    [<Test>]
    let ``the PAL composition answers exactly what the six-bit projection answered`` () : unit =
        let pal = pinnedPollEvents ()
        let alwaysReported = platform.["POLLERR"] ||| platform.["POLLHUP"]

        let mismatches =
            [
                for level in everyLevel do
                    for raw in 0..0xFFFF do
                        let palEvents = int16 (uint16 raw)

                        let kernel =
                            linuxBits level &&& (PollEventsPal.toPlatform palEvents ||| alwaysReported)

                        let reported = PollEventsPal.ofPlatform kernel
                        let expected = sixBitProjection pal level palEvents

                        if reported <> expected then
                            yield
                                $"%A{level}, PAL events 0x%04x{raw}: expected 0x%04x{uint16 expected}, got 0x%04x{uint16 reported}"

                        if (kernel <> 0s) <> (reported <> 0s) then
                            yield $"%A{level}, PAL events 0x%04x{raw}: the kernel reported 0x%04x{uint16 kernel}"
            ]

        mismatches |> List.truncate 20 |> shouldEqual []
