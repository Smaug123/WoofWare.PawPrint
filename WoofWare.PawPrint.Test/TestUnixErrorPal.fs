namespace WoofWare.PawPrint.Test

open System
open System.IO
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
                    $"TestUnixErrorPal: UnixError.%s{name} has no counterpart in the pinned Interop.Error enum, so PawPrint would be reporting a PAL value CoreLib never switches on."
            | Some expected -> UnixErrorPal.toPal error |> shouldEqual expected

    [<Test>]
    let ``PAL numbering is injective`` () : unit =
        let pals = UnixError.all |> List.map UnixErrorPal.toPal
        pals |> List.distinct |> List.length |> shouldEqual pals.Length

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
            | RawErrnoPortability.PlatformDependent _ ->
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
        // under Darwin is EMSGSIZE, which is not modelled, so it fails rather
        // than answering ELOOP.
        let exn =
            Assert.Throws<Exception> (fun () -> UnixErrorPal.ofRawErrnoUnder RawErrnoNumbering.Darwin 40 |> ignore<int>)

        exn.Message |> shouldContainText "no entry for it on this platform"
