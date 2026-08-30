namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `RealRuntime.validateSeedForOracle`: which seeds a differential case may
/// declare.
///
/// A seed is `WoofWare.PosixKernel`'s idea of a filesystem, and the interpreted
/// guest gets exactly the one it describes. The real runtime does not: the
/// oracle materialises the seed as a real directory on the host, so a seed
/// naming something a real directory cannot hold — or carrying a mode a host
/// `chmod` may silently drop — would leave the two runtimes answering questions
/// about different filesystems, while the comparison still ran and still looked
/// like evidence.
///
/// So this rule is the oracle's, not the seed type's: `TestFileSystemSeed` in
/// `WoofWare.PosixKernel.Test` says what a seed *is*, and these two rows say
/// which of those a comparison may use.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSeedForOracle =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private mode (raw : int) : PermissionBits =
        PermissionBits.parseOrFail "test seed" raw

    let private bytes (s : string) : ImmutableArray<byte> =
        System.Text.Encoding.UTF8.GetBytes s |> ImmutableArray.CreateRange

    [<Test>]
    let ``the oracle refuses a seed a real directory cannot stand in for`` () : unit =
        // Each refused shape is a way the differential comparison would still be
        // *made*, and would still look like evidence, while the two runtimes
        // answered questions about different filesystems.
        let reserved = [ "Guest.dll" ; "Guest.runtimeconfig.json" ]

        let refused (seed : Map<DirectoryEntryName, SeedEntry>) : string =
            let failure =
                Assert.Throws (fun () -> RealRuntime.validateSeedForOracle reserved seed)

            failure.Message

        // The control: exactly the shape the differential cases use — a file, a
        // nested directory, a link to a sibling, a dangling link, and a cycle.
        RealRuntime.validateSeedForOracle
            reserved
            (Map.ofList
                [
                    name "f", SeedEntry.file (bytes "hello")
                    name "d", SeedEntry.directory (Map.ofList [ name "g", SeedEntry.file (bytes "nested") ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ])

        // An absolute target: PawPrint resolves it against its own root, the
        // host against the real one.
        refused (Map.ofList [ name "l", SeedEntry.Symlink (target "/etc/passwd") ])
        |> shouldContainText "is absolute"

        // Anything with more than one component, which is where deciding
        // whether the host would leave the scratch directory stops being
        // possible by inspection: "x" may itself be a symlink, and ".." on the
        // host does not clamp at the root the way PawPrint's does.
        refused (Map.ofList [ name "l", SeedEntry.Symlink (target "../f") ])
        |> shouldContainText "not a single path component"

        refused (Map.ofList [ name "l", SeedEntry.Symlink (target "d/g") ])
        |> shouldContainText "not a single path component"

        refused (Map.ofList [ name "l", SeedEntry.Symlink (target "x/../victim") ])
        |> shouldContainText "not a single path component"

        // "." and ".." are components too, so they are refused by the same
        // rule rather than by a special case.
        refused (Map.ofList [ name "l", SeedEntry.Symlink (target ".") ])
        |> shouldContainText "not a single path component"

        // A target the seed does not declare is fine — the link dangles the
        // same way on both sides...
        RealRuntime.validateSeedForOracle reserved (Map.ofList [ name "l", SeedEntry.Symlink (target "nx") ])

        // ...unless a case-insensitive host would resolve it anyway, which
        // PawPrint would not.
        refused (
            Map.ofList
                [
                    name "f", SeedEntry.file (bytes "a")
                    name "l", SeedEntry.Symlink (target "F")
                ]
        )
        |> shouldContainText "case-insensitive host would resolve it"

        // ...including onto the guest image, which the oracle writes and
        // PawPrint's filesystem does not contain at all.
        refused (Map.ofList [ name "l", SeedEntry.Symlink (target "Guest.dll") ])
        |> shouldContainText "case-insensitive host would resolve it"

        // Names differing only by case: one file on a stock macOS, two in
        // PawPrint. Refused on every host, so that a seed cannot pass here and
        // compare the wrong thing in CI.
        refused (Map.ofList [ name "f", SeedEntry.file (bytes "a") ; name "F", SeedEntry.file (bytes "b") ])
        |> shouldContainText "case or Unicode normalisation"

        // ...at any depth, not just the root.
        refused (
            Map.ofList
                [
                    name "d",
                    SeedEntry.directory (
                        Map.ofList [ name "g", SeedEntry.file (bytes "a") ; name "G", SeedEntry.file (bytes "b") ]
                    )
                ]
        )
        |> shouldContainText "case or Unicode normalisation"

        // Anything outside the ASCII alphabet whose case folding is
        // unambiguous. A stock macOS filesystem folds Unicode normalisation and
        // applies *full* case folding — it aliases the two spellings of
        // "e-acute" below, and separately aliases "ss" with the sharp s, which
        // no simple lowering reproduces. Refused rather than approximated.
        let precomposed = "\u00E9"
        let decomposed = "e\u0301"
        precomposed |> shouldNotEqual decomposed

        refused (Map.ofList [ name precomposed, SeedEntry.file (bytes "a") ])
        |> shouldContainText "case folding is unambiguous"

        refused (Map.ofList [ name decomposed, SeedEntry.file (bytes "b") ])
        |> shouldContainText "case folding is unambiguous"

        refused (Map.ofList [ name "\u00DF", SeedEntry.file (bytes "a") ])
        |> shouldContainText "case folding is unambiguous"

        // The rule has to cover every string the folding is applied to, not
        // just the entry names: a sibling "ss" and a link targeting the sharp s
        // are distinct under `ToLowerInvariant` and aliased by APFS, so the
        // *target* needs the same alphabet.
        refused (
            Map.ofList
                [
                    name "ss", SeedEntry.file (bytes "a")
                    name "l", SeedEntry.Symlink (target "\u00DF")
                ]
        )
        |> shouldContainText "case folding is unambiguous"

        // ...but only when there is a seed at all. An unseeded run materialises
        // nothing, so a guest whose assembly is named "Paw Print" must still
        // run: there is nothing for the name to collide with.
        RealRuntime.validateSeedForOracle [ "Paw Print.dll" ; "Paw Print.runtimeconfig.json" ] Map.empty

        // ...as do the names the oracle itself contributes, which are derived
        // from the guest assembly's name.
        let reservedFailure =
            Assert.Throws (fun () ->
                RealRuntime.validateSeedForOracle
                    [ "Gu\u00DFest.dll" ]
                    (Map.ofList [ name "f", SeedEntry.file (bytes "a") ])
            )

        reservedFailure.Message |> shouldContainText "oracle's own reserved file"

        // ...while the alphabet the differential seeds actually use is fine.
        RealRuntime.validateSeedForOracle
            reserved
            (Map.ofList
                [
                    name "a-b_c.d", SeedEntry.file (bytes "x")
                    name "Guest2.dll", SeedEntry.file (bytes "y")
                ])

        // The guest image, which the oracle must write itself — including under
        // a case that a case-insensitive host would alias onto it.
        refused (Map.ofList [ name "Guest.dll", SeedEntry.file (bytes "a") ])
        |> shouldContainText "run the guest at all"

        refused (Map.ofList [ name "guest.DLL", SeedEntry.file (bytes "a") ])
        |> shouldContainText "run the guest at all"

    /// Split out from the shape-validation test above, and Unix-only, because
    /// on Windows the validator refuses *any* non-default mode first — it has no
    /// Unix bits to give a real file — so both halves below would be answered by
    /// that rule instead of the one under test.
    [<Test>]
    let ``the oracle refuses mode bits a host chmod may drop`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "The validator refuses every non-default mode on Windows, which is a different rule."

        let reserved = [ "Guest.dll" ; "Guest.runtimeconfig.json" ]

        let refused (seed : Map<DirectoryEntryName, SeedEntry>) : string =
            let failure =
                Assert.Throws (fun () -> RealRuntime.validateSeedForOracle reserved seed)

            failure.Message

        // Set-user-ID, set-group-ID and sticky: a host `chmod` may silently
        // drop these, so PawPrint would report a bit the host did not have and
        // the comparison would be about the harness rather than the runtimes.
        for special in [ 0o4644 ; 0o2644 ; 0o1644 ] do
            refused (Map.ofList [ name "f", SeedEntry.File (bytes "x", mode special) ])
            |> shouldContainText "set-user-ID/set-group-ID/sticky"

        // ...on a directory too, where the sticky bit is the one that actually
        // has a common use.
        refused (Map.ofList [ name "d", SeedEntry.Directory (Map.empty, mode 0o1755) ])
        |> shouldContainText "set-user-ID/set-group-ID/sticky"

        // The ordinary twelve-bit modes are all fine, including ones the
        // default umask would never produce. This half is what stops the rule
        // above being satisfied by a validator that simply refused every mode.
        for ordinary in [ 0o000 ; 0o400 ; 0o600 ; 0o666 ; 0o777 ] do
            RealRuntime.validateSeedForOracle
                reserved
                (Map.ofList [ name "f", SeedEntry.File (bytes "x", mode ordinary) ])
