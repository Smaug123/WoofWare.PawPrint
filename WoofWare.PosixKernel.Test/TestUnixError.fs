namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixError`'s table is a transcription of the kernel ABI headers, so the
/// tests that matter here re-derive it from the *host's own* `<errno.h>` rather
/// than restating it. (.NET's PAL numbering of these errors is a client's
/// business: it lives in `WoofWare.PawPrint.Test.TestUnixErrorPal`, checked
/// against its own upstream authority.)
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

    let private caseName (error : UnixError) : string = sprintf "%O" error

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

            match UnixError.rawNumbering error with
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
        match UnixError.rawNumbering error with
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

    /// Linux and Darwin agree on 1-34 except 11, where `EAGAIN` and `EDEADLK`
    /// are transposed, so a case claiming `Portable` must land inside that set.
    /// This is the check that stops a genuinely platform-dependent number being
    /// smuggled in as portable, which is the failure mode that would silently
    /// hand a guest the wrong errno.
    [<Test>]
    let ``every portable raw errno lies in the platform-independent range`` () : unit =
        for error in UnixError.all do
            match UnixError.rawNumbering error with
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
            match UnixError.rawNumbering error with
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
    let ``a portable errno is the same number under either numbering`` () : unit =
        // Two ways to reach a raw errno coexist -- `toRawErrno`, which answers
        // only where the platforms agree, and `toRawErrnoUnder`, which asks the
        // flavour. A handler moving from the first to the second must not change
        // what a guest reads, and for the portable errnos it cannot; asserted
        // rather than assumed, because "they agree" is the reason such a move is
        // safe and nothing else in the suite states it.
        for error in UnixError.all do
            match UnixError.rawNumbering error with
            | RawErrnoPortability.PlatformDependent _ -> ()
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
    let ``ofRawErrno maps neither candidate of a platform-dependent error`` () : unit =
        // Mapping 40 to ELOOP would be right on Linux and wrong on Darwin,
        // where 40 is EMSGSIZE; mapping 62 would be wrong the other way round.
        UnixError.ofRawErrno 40 |> shouldEqual None
        UnixError.ofRawErrno 62 |> shouldEqual None

    [<Test>]
    let ``ofRawErrno declines a platform-dependent errno`` () : unit =
        UnixError.ofRawErrno 39 |> shouldEqual None
        UnixError.ofRawErrno 11 |> shouldEqual None
        UnixError.ofRawErrno 2 |> shouldEqual (Some UnixError.ENOENT)
