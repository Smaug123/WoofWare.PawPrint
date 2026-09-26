namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixError`'s table is a transcription of each flavour's `<errno.h>`, so the
/// tests that matter here hold it to `ErrnoHeaders`, a checked-in transcription
/// of both flavours' headers, on every host. The host then checks that
/// transcription for its own flavour, against its header and its libc's
/// `strerror`: the Darwin half on a dev box, the Linux half in CI. (.NET's PAL
/// numbering of these errors is a client's business: it lives in
/// `WoofWare.PawPrint.Test.TestUnixErrorPal`, checked against its own upstream
/// authority.)
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixError =

    let private caseName (error : UnixError) : string = sprintf "%O" error

    let private numberings : RawErrnoNumbering list =
        [ RawErrnoNumbering.Linux ; RawErrnoNumbering.Darwin ]

    let private transcription (numbering : RawErrnoNumbering) : (string * int) list =
        match numbering with
        | RawErrnoNumbering.Linux -> ErrnoHeaders.linux
        | RawErrnoNumbering.Darwin -> ErrnoHeaders.darwin

    /// The names a header defines that are not an error of their own, each with
    /// the name whose number it must share. `ELAST` is Darwin's bound, "must be
    /// equal largest errno"; the rest are a second spelling of one error on
    /// every modelled Unix that has them, so a case for them would be a second
    /// value for one error.
    let private notAnError (numbering : RawErrnoNumbering) : Map<string, string> =
        match numbering with
        | RawErrnoNumbering.Linux -> Map.ofList [ "EWOULDBLOCK", "EAGAIN" ; "EDEADLOCK", "EDEADLK" ]
        | RawErrnoNumbering.Darwin -> Map.ofList [ "EWOULDBLOCK", "EAGAIN" ; "ELAST", "ENOTCAPABLE" ]

    /// Pairs of cases one numbering gives one number, each with the case that
    /// number decodes as. Only Linux's `ENOTSUP` and `EOPNOTSUPP`: they are one
    /// error there and two on Darwin, which is why both are cases.
    let private aliases (numbering : RawErrnoNumbering) : Map<UnixError, UnixError> =
        match numbering with
        | RawErrnoNumbering.Linux -> Map.ofList [ UnixError.ENOTSUP, UnixError.EOPNOTSUPP ]
        | RawErrnoNumbering.Darwin -> Map.empty

    /// The case a number encoded from `error` decodes back to.
    let private decodesAs (numbering : RawErrnoNumbering) (error : UnixError) : UnixError =
        Map.tryFind error (aliases numbering) |> Option.defaultValue error

    /// Every number a sweep puts to the decoder: every errno either flavour
    /// assigns and a margin well past the largest, the negatives just below
    /// zero, and the extremes.
    let private sweep : int list =
        [ Int32.MinValue ; -0x20002 ; -0x20001 ; Int32.MaxValue ] @ [ -300 .. 4096 ]

    // ---------------------------------------------------------------------
    // The table, against the checked-in transcription.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``each numbering's table is exactly its header's errors`` () : unit =
        for numbering in numberings do
            let skipped = notAnError numbering

            let expected =
                transcription numbering
                |> List.filter (fun (name, _) -> not (Map.containsKey name skipped))
                |> Set.ofList

            let actual =
                UnixError.all
                |> List.choose (fun error ->
                    UnixError.tryToRawErrnoUnder numbering error
                    |> Option.map (fun raw -> caseName error, raw)
                )
                |> Set.ofList

            let missing = Set.difference expected actual |> Set.toList
            let extra = Set.difference actual expected |> Set.toList

            if not (List.isEmpty missing && List.isEmpty extra) then
                failwith
                    $"TestUnixError: under %O{numbering}, the header defines %A{missing} which the table lacks, and the table has %A{extra} which the header does not define."

    [<Test>]
    let ``every header name that is not an error shares its number with the error it names`` () : unit =
        for numbering in numberings do
            let header = Map.ofList (transcription numbering)

            for KeyValue (name, target) in notAnError numbering do
                match Map.tryFind name header, Map.tryFind target header with
                | Some value, Some targetValue ->
                    if value <> targetValue then
                        failwith
                            $"TestUnixError: under %O{numbering}, %s{name} is %d{value} but %s{target} is %d{targetValue}; the header no longer makes the one a spelling of the other."
                | _ -> failwith $"TestUnixError: under %O{numbering}, the header lacks %s{name} or %s{target}."

    [<Test>]
    let ``Darwin's ELAST is its largest errno`` () : unit =
        let largest =
            UnixError.all
            |> List.choose (UnixError.tryToRawErrnoUnder RawErrnoNumbering.Darwin)
            |> List.max

        Map.ofList ErrnoHeaders.darwin |> Map.find "ELAST" |> shouldEqual largest

    // ---------------------------------------------------------------------
    // The encoding and decoding, as properties of the whole table.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``every error a numbering has decodes back from its own number`` () : unit =
        for numbering in numberings do
            for error in UnixError.all do
                match UnixError.tryToRawErrnoUnder numbering error with
                | None -> ()
                | Some raw ->
                    UnixError.ofRawErrnoUnder numbering raw
                    |> shouldEqual (Some (decodesAs numbering error))

    /// The converse: whatever a number decodes as encodes back to that number,
    /// and a number decodes as nothing exactly when no case has it.
    let private decodeThenEncode (numbering : RawErrnoNumbering) (raw : int) : unit =
        match UnixError.ofRawErrnoUnder numbering raw with
        | Some error -> UnixError.toRawErrnoUnder numbering error |> shouldEqual raw
        | None ->
            UnixError.all
            |> List.filter (fun error -> UnixError.tryToRawErrnoUnder numbering error = Some raw)
            |> shouldEqual []

    [<Test>]
    let ``every swept number decodes to an error that encodes back to it, or to nothing`` () : unit =
        for numbering in numberings do
            for raw in sweep do
                decodeThenEncode numbering raw

    [<Test>]
    let ``every int decodes to an error that encodes back to it, or to nothing`` () : unit =
        let property (raw : int) : unit =
            for numbering in numberings do
                decodeThenEncode numbering raw

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 1000, property)

    [<Test>]
    let ``an error a numbering lacks is refused by toRawErrnoUnder, naming it`` () : unit =
        let mutable refused = 0

        for numbering in numberings do
            for error in UnixError.all do
                match UnixError.tryToRawErrnoUnder numbering error with
                | Some _ -> ()
                | None ->
                    refused <- refused + 1

                    let exn =
                        Assert.Throws<Exception> (fun () -> UnixError.toRawErrnoUnder numbering error |> ignore<int>)

                    exn.Message |> shouldContainText (caseName error)
                    exn.Message |> shouldContainText (sprintf "%O" numbering)

        // Linux's forty-four and Darwin's nineteen, so the loop is not vacuous.
        refused |> shouldEqual 63

    [<Test>]
    let ``all lists every case exactly once`` () : unit =
        let cases = Reflection.FSharpType.GetUnionCases typeof<UnixError>

        UnixError.all
        |> List.distinct
        |> List.length
        |> shouldEqual (UnixError.all |> List.length)

        UnixError.all |> List.length |> shouldEqual cases.Length

    [<Test>]
    let ``raw errno numbering is injective on each platform, up to the declared aliases`` () : unit =
        // Injectivity has to hold per platform, not across the union: 40 is a
        // legitimate number for two different errors *on different platforms*,
        // and comparing the pooled values would report a collision that no
        // running system could ever observe.
        for numbering in numberings do
            let collisions =
                UnixError.all
                |> List.choose (fun error ->
                    UnixError.tryToRawErrnoUnder numbering error
                    |> Option.map (fun raw -> raw, error)
                )
                |> List.groupBy fst
                |> List.map (fun (raw, errors) -> raw, List.map snd errors)
                |> List.filter (fun (_, errors) -> List.length errors > 1)

            let declared =
                aliases numbering
                |> Map.toList
                |> List.map (fun (alias, canonical) -> Set.ofList [ alias ; canonical ])

            let undeclared =
                collisions
                |> List.filter (fun (_, errors) -> not (List.contains (Set.ofList errors) declared))

            if not (List.isEmpty undeclared) then
                failwith $"TestUnixError: raw errno numbering collides under %O{numbering}: %A{undeclared}"

            // And every declared alias really does collide, or the declaration
            // is stale.
            collisions |> List.length |> shouldEqual declared.Length

    /// Linux and Darwin agree on 1-34 except 11, where `EAGAIN` and `EDEADLK`
    /// are transposed, so a case claiming `Portable` must land inside that set,
    /// and every other case's numbers outside it. The first stops a
    /// platform-dependent number being smuggled in as portable; the second
    /// stops `toRawErrno` refusing a question that has a single answer.
    [<Test>]
    let ``exactly the portable errors lie in the platform-independent range`` () : unit =
        let agreed (raw : int) : bool = raw >= 1 && raw <= 34 && raw <> 11

        for error in UnixError.all do
            let isPortable, numbers =
                match UnixError.rawNumbering error with
                | RawErrnoPortability.Portable raw -> true, [ raw ]
                | RawErrnoPortability.PlatformDependent (linux, darwin) -> false, [ linux ; darwin ]
                | RawErrnoPortability.LinuxOnly linux -> false, [ linux ]
                | RawErrnoPortability.DarwinOnly darwin -> false, [ darwin ]

            for raw in numbers do
                if agreed raw <> isPortable then
                    let where = if agreed raw then "inside" else "outside"

                    failwith
                        $"TestUnixError: UnixError.%O{error} is %A{UnixError.rawNumbering error}, but %d{raw} is %s{where} the range Linux and Darwin agree on (1-34, excluding 11)."

            match UnixError.rawNumbering error with
            | RawErrnoPortability.PlatformDependent (linux, darwin) when linux = darwin ->
                failwith
                    $"TestUnixError: UnixError.%O{error} is recorded as PlatformDependent, but both halves are %d{linux}; it is Portable."
            | _ -> ()

    [<Test>]
    let ``isPortableRawErrno is exactly the numbers both numberings decode to one portable error`` () : unit =
        for raw in sweep do
            let expected =
                match
                    UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux raw,
                    UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin raw
                with
                | Some linux, Some darwin -> linux = darwin
                | _ -> false

            UnixError.isPortableRawErrno raw |> shouldEqual expected
            UnixError.ofRawErrno raw |> Option.isSome |> shouldEqual expected

    // ---------------------------------------------------------------------
    // The transcription, against this host.
    // ---------------------------------------------------------------------

    let private errnoDefine : Regex =
        Regex (@"^#define\s+(?<name>E[A-Z0-9]+)\s+(?<value>[0-9]+)", RegexOptions.Multiline)

    /// Every `#define E... <number>` reachable from the host's `<errno.h>`.
    /// Linux splits these across `asm-generic/errno-base.h` and
    /// `asm-generic/errno.h`; Darwin keeps them all in `sys/errno.h`. Returns
    /// `None` when no header can be found, which is the normal case inside a
    /// sandboxed build.
    let private hostErrnoValues () : Map<string, int> option =
        let candidates =
            [
                "/usr/include/sys/errno.h"
                "/usr/include/asm-generic/errno-base.h"
                "/usr/include/asm-generic/errno.h"
                "/usr/include/errno.h"
            ]
            |> List.filter File.Exists

        // The SDK on a Nix darwin box is not under /usr/include; SDKROOT is set
        // by the devshell's clang wrapper when it is present.
        let sdkCandidates =
            match Environment.GetEnvironmentVariable "SDKROOT" with
            | null
            | "" -> []
            | sdk ->
                [ Path.Combine (sdk, "usr", "include", "sys", "errno.h") ]
                |> List.filter File.Exists

        match candidates @ sdkCandidates with
        | [] -> None
        | headers ->
            headers
            |> Seq.collect (fun header ->
                errnoDefine.Matches (File.ReadAllText header)
                |> Seq.map (fun m -> m.Groups.["name"].Value, Int32.Parse m.Groups.["value"].Value)
            )
            |> Map.ofSeq
            |> Some

    let private hostNumbering (flavour : SimulatedUnixFlavour) : RawErrnoNumbering =
        SimulatedUnixPlatform.rawErrnoNumbering (HostPlatform.platformOf flavour)

    /// Every error this host's header defines with a literal number is in the
    /// transcription with that number. Not the converse: an older SDK lacks
    /// the newest errors (the Nix devshell's macOS 14.4 SDK has no
    /// `ENOTCAPABLE`), which says nothing about the kernel. `strerror` below
    /// is the converse.
    [<Test>]
    let ``the transcription agrees with this host's errno.h`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            match hostErrnoValues () with
            | None ->
                Assert.Ignore "No <errno.h> found on this host; the transcription cannot be checked against it here."
            | Some host ->

            host |> Map.count |> shouldBeGreaterThan 30

            let numbering = hostNumbering flavour
            let transcribed = Map.ofList (transcription numbering)

            for KeyValue (name, value) in host do
                // A bound, which moves whenever an error is added.
                if name <> "ELAST" then
                    match Map.tryFind name transcribed with
                    | Some expected when expected = value -> ()
                    | other ->
                        failwith
                            $"TestUnixError: this host's <errno.h> defines %s{name} as %d{value}, but the %O{numbering} transcription has %A{other}."
        )

    /// The running libc's `strerror`, through .NET's own wrapper of it.
    /// glibc says "Unknown error 41" for a number it has no text for, and
    /// Darwin's libc "Unknown error: 108".
    let private strerrorKnows (raw : int) : bool =
        not ((Marshal.GetPInvokeErrorMessage raw).StartsWith ("Unknown error", StringComparison.Ordinal))

    [<Test>]
    let ``this host's strerror names exactly the numbers the table decodes`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering = hostNumbering flavour

            // 0 is "no error", which libc names and the table does not.
            for raw in 1..4096 do
                let known = strerrorKnows raw
                let decoded = UnixError.ofRawErrnoUnder numbering raw

                if known <> Option.isSome decoded then
                    failwith
                        $"TestUnixError: this host's strerror(%d{raw}) is %s{Marshal.GetPInvokeErrorMessage raw}, but under %O{numbering} the table decodes it as %A{decoded}."
        )

    // ---------------------------------------------------------------------
    // Particular rows.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``a portable errno is the same number under either numbering`` () : unit =
        // Two ways to reach a raw errno coexist -- `toRawErrno`, which answers
        // only where the platforms agree, and `toRawErrnoUnder`, which asks the
        // flavour. A handler moving from the first to the second must not change
        // what a guest reads, and for the portable errnos it cannot; asserted
        // rather than assumed, because "they agree" is the reason such a move is
        // safe and nothing else in the suite states it.
        for error in UnixError.all do
            match UnixError.rawNumbering error with
            | RawErrnoPortability.PlatformDependent _
            | RawErrnoPortability.LinuxOnly _
            | RawErrnoPortability.DarwinOnly _ -> ()
            | RawErrnoPortability.Portable _ ->
                let portable = UnixError.toRawErrno error

                for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
                    UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error
                    |> shouldEqual portable

    [<Test>]
    let ``toRawErrno refuses a platform-dependent error, naming both candidates`` () : unit =
        // ELOOP is admitted despite having no answerable raw number, because a
        // client's own encoding of it may still be usable —
        // `TestUnixErrorPal` asserts that half.
        let exn =
            Assert.Throws<Exception> (fun () -> UnixError.toRawErrno UnixError.ELOOP |> ignore<int>)

        // Both numbers, so whoever hits this can see the choice being refused
        // rather than having to go and look it up.
        exn.Message |> shouldContainText "40"
        exn.Message |> shouldContainText "62"
        exn.Message |> shouldContainText "ELOOP"

    [<Test>]
    let ``toRawErrno refuses a one-flavour error, naming its flavour`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> UnixError.toRawErrno UnixError.ENOKEY |> ignore<int>)

        exn.Message |> shouldContainText "ENOKEY"
        exn.Message |> shouldContainText "only on Linux"
        exn.Message |> shouldContainText "126"

        let exn =
            Assert.Throws<Exception> (fun () -> UnixError.toRawErrno UnixError.EAUTH |> ignore<int>)

        exn.Message |> shouldContainText "EAUTH"
        exn.Message |> shouldContainText "only on Darwin"
        exn.Message |> shouldContainText "80"

    [<Test>]
    let ``ofRawErrno maps neither candidate of a platform-dependent error`` () : unit =
        // Mapping 40 to ELOOP would be right on Linux and wrong on Darwin,
        // where 40 is EMSGSIZE; mapping 62 would be wrong the other way round.
        UnixError.ofRawErrno 40 |> shouldEqual None
        UnixError.ofRawErrno 62 |> shouldEqual None

    /// The alias, from the decoding side: Linux's 95 is one errno, and it is
    /// answered as `EOPNOTSUPP`; Darwin's 45 and 102 are two.
    [<Test>]
    let ``the Linux alias decodes as EOPNOTSUPP, and Darwin's two numbers as two errors`` () : unit =
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 95
        |> shouldEqual (Some UnixError.EOPNOTSUPP)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 45
        |> shouldEqual (Some UnixError.ENOTSUP)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 102
        |> shouldEqual (Some UnixError.EOPNOTSUPP)

        // Both directions of the encode agree with that.
        UnixError.toRawErrnoUnder RawErrnoNumbering.Linux UnixError.ENOTSUP
        |> shouldEqual 95

        UnixError.toRawErrnoUnder RawErrnoNumbering.Linux UnixError.EOPNOTSUPP
        |> shouldEqual 95

    /// The errnos the library's own refusal messages cite as measured answers
    /// can be spelled.
    [<Test>]
    let ``the measured socket and lock errnos decode on both numberings`` () : unit =
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 107
        |> shouldEqual (Some UnixError.ENOTCONN)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 57
        |> shouldEqual (Some UnixError.ENOTCONN)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 35
        |> shouldEqual (Some UnixError.EDEADLK)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 11
        |> shouldEqual (Some UnixError.EDEADLK)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 40
        |> shouldEqual (Some UnixError.ELOOP)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 40
        |> shouldEqual (Some UnixError.EMSGSIZE)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 89
        |> shouldEqual (Some UnixError.EDESTADDRREQ)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 39
        |> shouldEqual (Some UnixError.EDESTADDRREQ)

    [<Test>]
    let ``EILSEQ is 84 on Linux and 92 on Darwin`` () : unit =
        UnixError.toRawErrnoUnder RawErrnoNumbering.Linux UnixError.EILSEQ
        |> shouldEqual 84

        UnixError.toRawErrnoUnder RawErrnoNumbering.Darwin UnixError.EILSEQ
        |> shouldEqual 92

        // Each number is a different error on the other platform.
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 84
        |> shouldEqual (Some UnixError.EOVERFLOW)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 92
        |> shouldEqual (Some UnixError.ENOPROTOOPT)

    /// Linux leaves 41 and 58 unassigned, and Darwin stops at 107.
    [<Test>]
    let ``an unassigned number decodes as nothing`` () : unit =
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 41 |> shouldEqual None
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 58 |> shouldEqual None
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux 134 |> shouldEqual None

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 107
        |> shouldEqual (Some UnixError.ENOTCAPABLE)

        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 108 |> shouldEqual None
        UnixError.ofRawErrnoUnder RawErrnoNumbering.Darwin 0 |> shouldEqual None

    [<Test>]
    let ``ofRawErrno declines a platform-dependent errno`` () : unit =
        UnixError.ofRawErrno 39 |> shouldEqual None
        UnixError.ofRawErrno 11 |> shouldEqual None
        UnixError.ofRawErrno 2 |> shouldEqual (Some UnixError.ENOENT)
        UnixError.ofRawErrno 15 |> shouldEqual (Some UnixError.ENOTBLK)
