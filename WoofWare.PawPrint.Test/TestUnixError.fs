namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `UnixError`'s table is a transcription of two upstream authorities, so the
/// tests that matter here re-derive it from those authorities rather than
/// restating it. The PAL column comes from the pinned `Interop.Errors.cs`; the
/// raw column is checked against the *host's own* `<errno.h>`, which is a valid
/// oracle on any platform precisely because the table claims those numbers are
/// platform-independent.
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

        // Every case we claim is portable must match *this* platform, whichever
        // platform that is. That is the whole content of the portability claim:
        // it has to hold on every host the suite ever runs on.
        for error in UnixError.all do
            let name = caseName error

            match Map.tryFind name host with
            | None -> () // Header split we did not find; the PAL test still covers the case.
            | Some expected ->
                let actual = UnixError.toRawErrno error

                if actual <> expected then
                    failwith
                        $"TestUnixError: UnixError.%s{name} is recorded as raw errno %d{actual}, but this host's <errno.h> defines it as %d{expected}. UnixError only admits errnos whose number is the same on every Unix PawPrint models, so either the table is wrong or %s{name} does not belong in it."

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

    [<Test>]
    let ``raw errno numbering is injective`` () : unit =
        let raws = UnixError.all |> List.map UnixError.toRawErrno
        raws |> List.distinct |> List.length |> shouldEqual raws.Length

    [<Test>]
    let ``PAL numbering is injective`` () : unit =
        let pals = UnixError.all |> List.map UnixError.toPal
        pals |> List.distinct |> List.length |> shouldEqual pals.Length

    /// The membership rule stated in `UnixError`'s doc comment. Linux and Darwin
    /// agree on 1-34 except 11, where `EAGAIN` and `EDEADLK` are transposed, so a
    /// case whose raw number falls outside that set cannot honestly claim to be
    /// portable.
    [<Test>]
    let ``every raw errno lies in the platform-independent range`` () : unit =
        for error in UnixError.all do
            let raw = UnixError.toRawErrno error

            if raw < 1 || raw > 34 || raw = 11 then
                failwith
                    $"TestUnixError: UnixError.%O{error} is recorded as raw errno %d{raw}, which is outside the range Linux and Darwin agree on (1-34, excluding 11 where EAGAIN and EDEADLK are transposed). Such a number is platform-dependent, so it must not be represented as RawErrnoPortability.Portable."

    // ---------------------------------------------------------------------
    // Conversion behaviour.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``palOfRawErrno maps zero to SUCCESS`` () : unit =
        UnixError.palOfRawErrno 0 |> shouldEqual UnixError.palSuccess

    [<Test>]
    let ``palOfRawErrno inverts toRawErrno`` () : unit =
        for error in UnixError.all do
            UnixError.palOfRawErrno (UnixError.toRawErrno error)
            |> shouldEqual (UnixError.toPal error)

    /// ENOTBLK is 15 on both Linux and Darwin, so its meaning needs no platform
    /// choice — but `Interop.Error` has no entry for it, so upstream's switch
    /// falls through to ENONSTANDARD. We must do the same rather than crash:
    /// this conversion is unambiguous, it simply has no PAL name. Today this is
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

    /// The crux of the design: an errno whose meaning depends on the platform
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
