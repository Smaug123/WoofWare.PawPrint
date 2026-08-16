namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `UnixError`'s table is a transcription of two upstream authorities, so the
/// tests that matter here re-derive it from those authorities rather than
/// restating it. The PAL column comes from the pinned `Interop.Errors.cs`; the
/// raw column is checked against the *host's own* `<errno.h>`.
///
/// The host header is a valid oracle for a `Portable` case precisely because
/// the table claims its number is platform-independent — so it must match
/// whichever platform the suite happens to run on.
///
/// A `PlatformDependent` case is weaker. The
/// host header can only check the *half* corresponding to the platform the
/// suite is running on: the Darwin half on a dev box, the Linux half in CI.
/// The other half is held only by a literal in
/// `toRawErrno refuses a platform-dependent error`, which catches drift but is
/// a restatement of the table rather than an independent authority. Checking
/// both halves everywhere would mean pinning a second platform's `<errno.h>`
/// in `flake.nix`; nothing in the repository can currently serve as that
/// oracle, because upstream's own conversion switches on `<errno.h>` *symbols*
/// rather than numbers.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixError =

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

    /// `EPERM = 0x10042,   // Operation not permitted.` and friends. Deliberately
    /// anchored to the enum's indentation so that the `EOPNOTSUPP = ENOTSUP`
    /// aliases at the end of the enum — which have no numeric literal — do not
    /// match.
    let private palEntry : Regex =
        Regex (@"^\s+(?<name>E[A-Z0-9]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),", RegexOptions.Multiline)

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
                $"TestUnixError: expected the pinned PAL error enum at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common/src/Interop/Unix, UnixError's PAL column has lost its oracle."

        palEntry.Matches (File.ReadAllText path)
        |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
        |> Map.ofSeq

    [<Test>]
    let ``PAL values agree with the pinned Interop.Errors.cs`` () : unit =
        let pinned = pinnedPalValues ()

        // Guard against the regex silently matching nothing and the test then
        // passing vacuously.
        pinned |> Map.count |> shouldBeGreaterThan 50

        for error in UnixError.all do
            let name = caseName error

            match Map.tryFind name pinned with
            | None ->
                failwith
                    $"TestUnixError: UnixError.%s{name} has no counterpart in the pinned Interop.Error enum, so PawPrint would be reporting a PAL value CoreLib never switches on."
            | Some expected -> UnixError.toPal error |> shouldEqual expected

    // ---------------------------------------------------------------------
    // The raw column, against the host's own <errno.h>.
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

    [<Test>]
    let ``raw errno values agree with the host's errno.h`` () : unit =
        match hostErrnoValues () with
        | None ->
            Assert.Ignore
                "No <errno.h> found on this host; the raw column's cross-platform claim cannot be checked here."
        | Some host ->

        host |> Map.count |> shouldBeGreaterThan 30

        let isDarwin = RuntimeInformation.IsOSPlatform OSPlatform.OSX

        for error in UnixError.all do
            let name = caseName error

            match Map.tryFind name host with
            | None -> () // Header split we did not find; the PAL test still covers the case.
            | Some expected ->

            match (UnixError.numbering error).Raw with
            | RawErrnoPortability.Portable actual ->
                // The whole content of the portability claim: it has to hold on
                // every host the suite ever runs on.
                if actual <> expected then
                    failwith
                        $"TestUnixError: UnixError.%s{name} is recorded as the portable raw errno %d{actual}, but this host's <errno.h> defines it as %d{expected}. Either the table is wrong, or %s{name} is not portable after all and belongs in RawErrnoPortability.PlatformDependent."
            | RawErrnoPortability.PlatformDependent (linux, darwin) ->
                let actual = if isDarwin then darwin else linux
                let half = if isDarwin then "Darwin" else "Linux"

                if actual <> expected then
                    failwith
                        $"TestUnixError: UnixError.%s{name} records %s{half} raw errno %d{actual}, but this host's <errno.h> defines it as %d{expected}."

                // The claim that made this case PlatformDependent rather than
                // Portable is that the two halves genuinely differ. If they
                // agreed it should have been Portable, and `toRawErrno` would be
                // refusing to answer a question that has an answer.
                if linux = darwin then
                    failwith
                        $"TestUnixError: UnixError.%s{name} is recorded as PlatformDependent, but both halves are %d{linux}. A number that is the same on both platforms is Portable; as written, toRawErrno refuses an answerable question."

    // ---------------------------------------------------------------------
    // Structural invariants of the table itself.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``all lists every case exactly once`` () : unit =
        let cases = Reflection.FSharpType.GetUnionCases typeof<UnixError>

        UnixError.all
        |> List.distinct
        |> List.length
        |> shouldEqual (UnixError.all |> List.length)

        UnixError.all |> List.length |> shouldEqual cases.Length

    /// The raw errno a given platform would report, whether or not PawPrint is
    /// willing to pick one. Only for the structural checks below: production
    /// code goes through `toRawErrno`, which refuses rather than choosing.
    let private rawOn (darwin : bool) (error : UnixError) : int =
        match (UnixError.numbering error).Raw with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwinValue) -> if darwin then darwinValue else linux

    [<Test>]
    let ``raw errno numbering is injective on each platform separately`` () : unit =
        // Injectivity has to hold per platform, not across the union: 40 is a
        // legitimate number for two different errors *on different platforms*,
        // and comparing the pooled values would report a collision that no
        // running system could ever observe.
        for darwin in [ true ; false ] do
            let raws = UnixError.all |> List.map (rawOn darwin)

            if List.length (List.distinct raws) <> List.length raws then
                let platform = if darwin then "Darwin" else "Linux"

                let collisions =
                    UnixError.all
                    |> List.groupBy (rawOn darwin)
                    |> List.filter (fun (_, errors) -> List.length errors > 1)
                    |> List.map (fun (raw, errors) ->
                        let names = errors |> List.map caseName |> String.concat "/"
                        $"%d{raw} is claimed by %s{names}"
                    )
                    |> String.concat "; "

                failwith $"TestUnixError: raw errno numbering collides on %s{platform}: %s{collisions}"

    [<Test>]
    let ``PAL numbering is injective`` () : unit =
        let pals = UnixError.all |> List.map UnixError.toPal
        pals |> List.distinct |> List.length |> shouldEqual pals.Length

    /// Linux and Darwin agree on 1-34 except 11, where `EAGAIN` and `EDEADLK`
    /// are transposed, so a case claiming `Portable` must land inside that set.
    /// This is the check that stops a genuinely platform-dependent number being
    /// smuggled in as portable, which is the failure mode that would silently
    /// hand a guest the wrong errno.
    [<Test>]
    let ``every portable raw errno lies in the platform-independent range`` () : unit =
        for error in UnixError.all do
            match (UnixError.numbering error).Raw with
            | RawErrnoPortability.PlatformDependent _ -> ()
            | RawErrnoPortability.Portable raw ->
                if raw < 1 || raw > 34 || raw = 11 then
                    failwith
                        $"TestUnixError: UnixError.%O{error} is recorded as the portable raw errno %d{raw}, which is outside the range Linux and Darwin agree on (1-34, excluding 11 where EAGAIN and EDEADLK are transposed). Such a number is platform-dependent, so it must be represented as RawErrnoPortability.PlatformDependent instead."

    /// The converse, and the reason `PlatformDependent` is not just a licence to
    /// skip the rule above: a number *inside* the agreed range has no business
    /// claiming to be platform-dependent, because `toRawErrno` would then refuse
    /// to answer a question that has a single correct answer.
    [<Test>]
    let ``no platform-dependent raw errno lies inside the agreed range`` () : unit =
        for error in UnixError.all do
            match (UnixError.numbering error).Raw with
            | RawErrnoPortability.Portable _ -> ()
            | RawErrnoPortability.PlatformDependent (linux, darwin) ->
                for raw, platform in [ linux, "Linux" ; darwin, "Darwin" ] do
                    if raw >= 1 && raw <= 34 && raw <> 11 then
                        failwith
                            $"TestUnixError: UnixError.%O{error} records %s{platform} raw errno %d{raw}, which is inside the range Linux and Darwin agree on, so it is answerable and the case should be Portable."

    // ---------------------------------------------------------------------
    // Conversion behaviour.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``palOfRawErrno maps zero to SUCCESS`` () : unit =
        UnixError.palOfRawErrno 0 |> shouldEqual UnixError.palSuccess

    [<Test>]
    let ``palOfRawErrno inverts toRawErrno wherever toRawErrno answers`` () : unit =
        for error in UnixError.all do
            match (UnixError.numbering error).Raw with
            | RawErrnoPortability.PlatformDependent _ ->
                // Not invertible, and deliberately so: see the refusal test
                // below, which drives both of ELOOP's candidate numbers.
                ()
            | RawErrnoPortability.Portable _ ->
                UnixError.palOfRawErrno (UnixError.toRawErrno error)
                |> shouldEqual (UnixError.toPal error)

    [<Test>]
    let ``toRawErrno refuses a platform-dependent error, naming both candidates`` () : unit =
        // The whole point of admitting ELOOP: its PAL value is usable, and is
        // what CoreLib switches on, but its raw number is not answerable.
        UnixError.toPal UnixError.ELOOP |> shouldEqual 0x10020

        let exn =
            Assert.Throws<Exception> (fun () -> UnixError.toRawErrno UnixError.ELOOP |> ignore<int>)

        // Both numbers, so whoever hits this can see the choice being refused
        // rather than having to go and look it up.
        exn.Message |> shouldContainText "40"
        exn.Message |> shouldContainText "62"
        exn.Message |> shouldContainText "ELOOP"

    [<Test>]
    let ``ofRawErrno maps neither candidate of a platform-dependent error`` () : unit =
        // Mapping 40 to ELOOP would be right on Linux and wrong on Darwin,
        // where 40 is EMSGSIZE; mapping 62 would be wrong the other way round.
        UnixError.ofRawErrno 40 |> shouldEqual None
        UnixError.ofRawErrno 62 |> shouldEqual None

    /// ENOTBLK is 15 on both Linux and Darwin, so its meaning needs no platform
    /// choice — but `Interop.Error` has no entry for it, so upstream's switch
    /// falls through to ENONSTANDARD. We must do the same rather than crash:
    /// this conversion is unambiguous, it just has no PAL name. Today this is
    /// the only raw errno in that class.
    [<Test>]
    let ``palOfRawErrno reports ENONSTANDARD for a portable errno with no PAL name`` () : unit =
        UnixError.palOfRawErrno 15 |> shouldEqual UnixError.palNonStandard

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
    let ``palOfRawErrno reports ENONSTANDARD for a negative errno`` (raw : int) : unit =
        UnixError.palOfRawErrno raw |> shouldEqual UnixError.palNonStandard

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
    let ``palOfRawErrno refuses a platform-dependent errno`` (raw : int) : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> UnixError.palOfRawErrno raw |> ignore<int>)

        exn.Message |> shouldContainText "platform-dependent"

    [<Test>]
    let ``ofRawErrno declines a platform-dependent errno`` () : unit =
        UnixError.ofRawErrno 39 |> shouldEqual None
        UnixError.ofRawErrno 11 |> shouldEqual None
        UnixError.ofRawErrno 2 |> shouldEqual (Some UnixError.ENOENT)
