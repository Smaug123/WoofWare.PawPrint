namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A seed is a *tree*, so the properties a path-list manifest would need —
/// order-independence, no duplicate paths, no child declared before its parent
/// — are not tested here, because they are not expressible. What is left to
/// check is that realising a seed produces the filesystem it describes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileSystemSeed =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private bytes (s : string) : ImmutableArray<byte> =
        System.Text.Encoding.UTF8.GetBytes s |> ImmutableArray.CreateRange

    /// A distinctive moment, so that "the seed recorded the time it was given"
    /// is distinguishable from "the seed left a default in place": the epoch is
    /// `Unchecked.defaultof<UnixTimestamp>`.
    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 250_000_000

    let private realise (seed : Map<FileName, SeedEntry>) : VirtualFileSystem =
        FileSystemSeed.toVirtualFileSystem createdAt seed

    // ----------------------------------------------------------------- basics

    [<Test>]
    let ``an empty seed is an empty filesystem`` () : unit =
        let vfs = realise FileSystemSeed.empty

        VirtualFileSystem.inodes vfs |> Map.count |> shouldEqual 1
        VirtualFileSystem.checkInvariants vfs |> shouldEqual []

    [<Test>]
    let ``a seed's contents are reachable at the paths it describes`` () : unit =
        let seed =
            Map.ofList
                [
                    name "etc",
                    SeedEntry.Directory (
                        Map.ofList
                            [
                                name "hostname", SeedEntry.File (bytes "pawprint")
                                name "localtime", SeedEntry.Symlink (target "/usr/share/zoneinfo/UTC")
                            ]
                    )
                    name "empty", SeedEntry.Directory Map.empty
                ]

        let vfs = realise seed
        let root = VirtualFileSystem.root vfs

        let contentAt (p : string) (policy : SymlinkPolicy) : InodeContent =
            match VirtualFileSystem.resolveExisting root policy (path p) vfs with
            | Error error -> failwith $"%s{p} did not resolve: %O{error}"
            | Ok inode ->

            match VirtualFileSystem.tryGetContent inode vfs with
            | Some content -> content
            | None -> failwith $"%s{p} resolved to an inode the graph does not contain"

        match contentAt "/etc/hostname" SymlinkPolicy.Follow with
        | InodeContent.RegularFile (contents, permissions) ->
            contents |> Seq.toArray |> shouldEqual (bytes "pawprint" |> Seq.toArray)
            permissions |> shouldEqual PermissionBits.defaultForRegularFile
        | other -> failwith $"expected a regular file, got %A{other}"

        match contentAt "/etc/localtime" SymlinkPolicy.NoFollowFinal with
        | InodeContent.Symlink stored ->
            // Verbatim, and in particular *not* resolved when the seed was
            // realised: this target names nothing the seed declares.
            SymlinkTarget.toString stored |> shouldEqual "/usr/share/zoneinfo/UTC"
        | other -> failwith $"expected a symlink, got %A{other}"

        match contentAt "/empty" SymlinkPolicy.Follow with
        | InodeContent.Directory directory ->
            directory.Entries |> Map.isEmpty |> shouldEqual true
            directory.Permissions |> shouldEqual PermissionBits.defaultForDirectory
        | other -> failwith $"expected a directory, got %A{other}"

        // ...and nothing the seed did not describe exists.
        VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/etc/passwd") vfs
        |> shouldEqual (Error UnixError.ENOENT)

    [<Test>]
    let ``a seeded directory's parent is the directory that holds it`` () : unit =
        // The "physical parent" link is what ".." walks, and nothing in the
        // seed's own shape supplies it — `createDirectory` does. A seed that
        // built the tree by binding inodes directly could get this wrong and
        // still look right until a guest wrote "..".
        let seed =
            Map.ofList
                [
                    name "a", SeedEntry.Directory (Map.ofList [ name "b", SeedEntry.Directory Map.empty ])
                ]

        let vfs = realise seed
        let root = VirtualFileSystem.root vfs

        VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/a/b/..") vfs
        |> shouldEqual (VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/a") vfs)

        VirtualFileSystem.resolveExisting root SymlinkPolicy.Follow (path "/a/b/../..") vfs
        |> shouldEqual (Ok root)

    [<Test>]
    let ``every seeded inode is created at the moment the seed was realised`` () : unit =
        let seed =
            Map.ofList
                [
                    name "f", SeedEntry.File (bytes "x")
                    name "l", SeedEntry.Symlink (target "f")
                    name "d", SeedEntry.Directory (Map.ofList [ name "g", SeedEntry.File ImmutableArray<byte>.Empty ])
                ]

        let vfs = realise seed

        for _, inode in Map.toList (VirtualFileSystem.inodes vfs) do
            inode.Times.Birth |> shouldEqual createdAt
            inode.Times.Access |> shouldEqual createdAt

            // Including the directories, whose mtime moved as each child was
            // bound — to the same instant, since the whole tree is one moment.
            inode.Times.Modification |> shouldEqual createdAt
            inode.Times.StatusChange |> shouldEqual createdAt

    // -------------------------------------------------------------- generated

    let private nameGen : Gen<FileName> =
        Gen.elements [ "a" ; "b" ; "c" ; "d" ] |> Gen.map name

    let private entryGen (depth : int) : Gen<SeedEntry> =
        let rec go (depth : int) : Gen<SeedEntry> =
            if depth <= 0 then
                Gen.oneof
                    [
                        Gen.constant (SeedEntry.File ImmutableArray<byte>.Empty)
                        Gen.constant (SeedEntry.Symlink (target "a"))
                        Gen.constant (SeedEntry.Directory Map.empty)
                    ]
            else
                Gen.oneof
                    [
                        Gen.constant (SeedEntry.File ImmutableArray<byte>.Empty)
                        Gen.constant (SeedEntry.Symlink (target "../a"))
                        Gen.zip nameGen (go (depth - 1))
                        |> Gen.listOf
                        |> Gen.map (Map.ofList >> SeedEntry.Directory)
                    ]

        go depth

    let private seedGen : Gen<Map<FileName, SeedEntry>> =
        Gen.zip nameGen (entryGen 3) |> Gen.listOf |> Gen.map Map.ofList

    /// Every absolute path the seed declares, paired with what it declares
    /// there. Computed from the seed rather than from the filesystem, so that
    /// it is an independent statement of what should be there.
    let rec private declared (prefix : string) (entries : Map<FileName, SeedEntry>) : (string * SeedEntry) list =
        entries
        |> Map.toList
        |> List.collect (fun (name, entry) ->
            let here = prefix + "/" + FileName.toString name

            match entry with
            | SeedEntry.Directory children -> (here, entry) :: declared here children
            | SeedEntry.File _
            | SeedEntry.Symlink _ -> [ here, entry ]
        )

    [<Test>]
    let ``every declared path resolves to what the seed declared there`` () : unit =
        let mutable observedDirectories = 0
        let mutable observedLeaves = 0

        let property (seed : Map<FileName, SeedEntry>) : unit =
            let vfs = realise seed
            let root = VirtualFileSystem.root vfs

            for declaredPath, entry in declared "" seed do
                // NoFollowFinal, because a declared symlink is the link itself
                // rather than whatever it happens to name.
                let inode =
                    match
                        VirtualFileSystem.resolveExisting root SymlinkPolicy.NoFollowFinal (path declaredPath) vfs
                    with
                    | Ok inode -> inode
                    | Error error -> failwith $"%s{declaredPath} was declared but did not resolve: %O{error}"

                match VirtualFileSystem.tryGetContent inode vfs, entry with
                | Some (InodeContent.RegularFile _), SeedEntry.File _ -> observedLeaves <- observedLeaves + 1
                | Some (InodeContent.Symlink _), SeedEntry.Symlink _ -> observedLeaves <- observedLeaves + 1
                | Some (InodeContent.Directory _), SeedEntry.Directory _ ->
                    observedDirectories <- observedDirectories + 1
                | actual, _ -> failwith $"%s{declaredPath} was declared as %A{entry} but resolved to %A{actual}"

        Check.One (config, Prop.forAll (Arb.fromGen seedGen) property)

        // Without these the property is satisfied by an empty seed every time.
        observedDirectories |> shouldBeGreaterThan 50
        observedLeaves |> shouldBeGreaterThan 50

    [<Test>]
    let ``realising a seed is deterministic, down to the inode numbers`` () : unit =
        // Inode numbers are guest-observable through `st_ino`, and the BCL
        // compares `(st_dev, st_ino)` pairs to decide whether two paths name
        // one file. So "the same seed gives the same numbers" is part of the
        // replay contract, not an implementation detail — and it is the reason
        // the realiser folds over `Map`, whose iteration order is the keys'
        // rather than the host's insertion order.
        let property (seed : Map<FileName, SeedEntry>) : unit =
            let first = realise seed
            let second = realise seed

            VirtualFileSystem.inodes first |> shouldEqual (VirtualFileSystem.inodes second)

            VirtualFileSystem.nextInode first
            |> shouldEqual (VirtualFileSystem.nextInode second)

        Check.One (config, Prop.forAll (Arb.fromGen seedGen) property)

    [<Test>]
    let ``a realised seed is always a filesystem a kernel could produce`` () : unit =
        let property (seed : Map<FileName, SeedEntry>) : unit =
            VirtualFileSystem.checkInvariants (realise seed) |> shouldEqual []

        Check.One (config, Prop.forAll (Arb.fromGen seedGen) property)

    // ------------------------------------------------------ the kernel's side

    [<Test>]
    let ``a boot clock in milliseconds becomes a timespec`` () : unit =
        // The seed's creation instant comes from `KernelConfig.WallClockEpochMs`,
        // so the millisecond-to-timespec conversion is on the path of every
        // seeded inode's mtime.
        let at (ms : int64) =
            UnixTimestamp.ofMillisecondsSinceEpoch ms

        at 0L |> shouldEqual UnixTimestamp.epoch

        at 1_700_000_123L
        |> shouldEqual (UnixTimestamp.createOrFail "test" 1_700_000L 123_000_000)

        at 999L |> shouldEqual (UnixTimestamp.createOrFail "test" 0L 999_000_000)
        at 1000L |> shouldEqual (UnixTimestamp.ofSeconds 1L)

        // Floor division, not truncation: a negative millisecond count must
        // keep the nanosecond part non-negative, because that is the only
        // `timespec` a kernel would ever write. Truncating toward zero would
        // give (0, -1_000_000) here, which `UnixTimestamp.create` refuses to
        // represent at all — so a bug would surface as a crash, but in a place
        // far from the arithmetic that caused it.
        at -1L |> shouldEqual (UnixTimestamp.createOrFail "test" -1L 999_000_000)
        at -1000L |> shouldEqual (UnixTimestamp.ofSeconds -1L)
        at -1001L |> shouldEqual (UnixTimestamp.createOrFail "test" -2L 999_000_000)

        // The bottom of the range, where the obvious way to floor a negative
        // — biasing the dividend by 999 before dividing — overflows without
        // throwing, and hands back a *positive* second count with a nanosecond
        // part outside [0, 1e9). That value would break `UnixTimestamp`'s own
        // invariant while never passing through `create`, so nothing downstream
        // would catch it.
        at Int64.MinValue
        |> shouldEqual (UnixTimestamp.createOrFail "test" -9_223_372_036_854_776L 192_000_000)

        at (Int64.MinValue + 1L)
        |> shouldEqual (UnixTimestamp.createOrFail "test" -9_223_372_036_854_776L 193_000_000)

        // ...and the invariant really does hold across the whole bottom edge,
        // stated as the property rather than as three more examples.
        for offset in 0L .. 1200L do
            let timestamp = at (Int64.MinValue + offset)
            UnixTimestamp.nanoseconds timestamp |> shouldBeSmallerThan 1_000_000_000
            UnixTimestamp.nanoseconds timestamp |> shouldBeGreaterThan -1
            UnixTimestamp.seconds timestamp |> shouldBeSmallerThan 0L

    // ------------------------------------------------------- the oracle's side

    [<Test>]
    let ``the oracle refuses a seed a real directory cannot stand in for`` () : unit =
        // These are not fussiness. Each is a way the differential comparison
        // would still be *made*, and would still look like evidence, while the
        // two runtimes answered questions about different filesystems.
        let reserved = [ "Guest.dll" ; "Guest.runtimeconfig.json" ]

        let refused (seed : Map<FileName, SeedEntry>) : string =
            let failure =
                Assert.Throws (fun () -> RealRuntime.validateSeedForOracle reserved seed)

            failure.Message

        // The control: exactly the shape the differential cases use — a file, a
        // nested directory, a link to a sibling, a dangling link, and a cycle.
        RealRuntime.validateSeedForOracle
            reserved
            (Map.ofList
                [
                    name "f", SeedEntry.File (bytes "hello")
                    name "d", SeedEntry.Directory (Map.ofList [ name "g", SeedEntry.File (bytes "nested") ])
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
                    name "f", SeedEntry.File (bytes "a")
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
        refused (Map.ofList [ name "f", SeedEntry.File (bytes "a") ; name "F", SeedEntry.File (bytes "b") ])
        |> shouldContainText "case or Unicode normalisation"

        // ...at any depth, not just the root.
        refused (
            Map.ofList
                [
                    name "d",
                    SeedEntry.Directory (
                        Map.ofList [ name "g", SeedEntry.File (bytes "a") ; name "G", SeedEntry.File (bytes "b") ]
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

        refused (Map.ofList [ name precomposed, SeedEntry.File (bytes "a") ])
        |> shouldContainText "case folding is unambiguous"

        refused (Map.ofList [ name decomposed, SeedEntry.File (bytes "b") ])
        |> shouldContainText "case folding is unambiguous"

        refused (Map.ofList [ name "\u00DF", SeedEntry.File (bytes "a") ])
        |> shouldContainText "case folding is unambiguous"

        // The rule has to cover every string the folding is applied to, not
        // just the entry names: a sibling "ss" and a link targeting the sharp s
        // are distinct under `ToLowerInvariant` and aliased by APFS, so the
        // *target* needs the same alphabet.
        refused (
            Map.ofList
                [
                    name "ss", SeedEntry.File (bytes "a")
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
                    (Map.ofList [ name "f", SeedEntry.File (bytes "a") ])
            )

        reservedFailure.Message |> shouldContainText "oracle's own reserved file"

        // ...while the alphabet the differential seeds actually use is fine.
        RealRuntime.validateSeedForOracle
            reserved
            (Map.ofList
                [
                    name "a-b_c.d", SeedEntry.File (bytes "x")
                    name "Guest2.dll", SeedEntry.File (bytes "y")
                ])

        // The guest image, which the oracle must write itself — including under
        // a case that a case-insensitive host would alias onto it.
        refused (Map.ofList [ name "Guest.dll", SeedEntry.File (bytes "a") ])
        |> shouldContainText "run the guest at all"

        refused (Map.ofList [ name "guest.DLL", SeedEntry.File (bytes "a") ])
        |> shouldContainText "run the guest at all"

    [<Test>]
    let ``every platform can answer every question stat asks of it`` () : unit =
        // The point of carrying a flavour: these three used to have a failure
        // arm for a platform that named only a release string, and a guest on
        // such a platform could abort the interpreter by stat-ing a symlink.
        // There is no such platform to have an arm for now.
        let linux = SimulatedUnixPlatform.linuxX64
        let darwin = SimulatedUnixPlatform.macOsArm64

        SimulatedUnixPlatform.symlinkPermissions linux
        |> PermissionBits.toInt
        |> shouldEqual 0o777

        // Measured: macOS applies the creating umask to a symlink, so 0o755
        // under the umask 022 PawPrint assumes until it models one.
        SimulatedUnixPlatform.symlinkPermissions darwin
        |> PermissionBits.toInt
        |> shouldEqual 0o755

        SimulatedUnixPlatform.reportsBirthTime linux |> shouldEqual false
        SimulatedUnixPlatform.reportsBirthTime darwin |> shouldEqual true

        SimulatedUnixPlatform.rawErrnoNumbering linux
        |> shouldEqual RawErrnoNumbering.Linux

        SimulatedUnixPlatform.rawErrnoNumbering darwin
        |> shouldEqual RawErrnoNumbering.Darwin

        // ...and a custom release answers them from its flavour, rather than
        // declining to answer at all.
        let custom =
            SimulatedUnixPlatform.createOrFail "test" SimulatedUnixFlavour.Linux "5.4.0-1234-custom"

        SimulatedUnixPlatform.unixRelease custom |> shouldEqual "5.4.0-1234-custom"
        SimulatedUnixPlatform.reportsBirthTime custom |> shouldEqual false

        SimulatedUnixPlatform.rawErrnoNumbering custom
        |> shouldEqual RawErrnoNumbering.Linux

        SimulatedUnixPlatform.symlinkPermissions custom
        |> PermissionBits.toInt
        |> shouldEqual 0o777

        // The presets are exactly their flavour plus their release, so a custom
        // platform that restates one *is* it. Under the shape this replaced,
        // the same platform had two spellings that compared unequal.
        SimulatedUnixPlatform.createOrFail "test" SimulatedUnixFlavour.Linux "6.8.0-51-generic"
        |> shouldEqual linux

        SimulatedUnixPlatform.createOrFail "test" SimulatedUnixFlavour.Darwin "24.6.0"
        |> shouldEqual darwin

    [<Test>]
    let ``a release string must be one a real uname could print`` () : unit =
        // Validated at construction rather than when the release is read, which
        // is what makes every accessor total — and means a host sees the
        // complaint next to the knob it set, rather than at the guest's first
        // `Environment.OSVersion`.
        let create (release : string) =
            SimulatedUnixPlatform.create SimulatedUnixFlavour.Linux release

        create "" |> shouldEqual (Error SimulatedUnixReleaseError.Empty)
        create null |> shouldEqual (Error SimulatedUnixReleaseError.Empty)

        // 255 is the looser of the two platforms' limits (macOS's
        // `_SYS_NAMELEN`; Linux's `_UTSNAME_LENGTH` is only 65).
        create (String.replicate 255 "a") |> Result.isOk |> shouldEqual true

        create (String.replicate 256 "a")
        |> shouldEqual (Error (SimulatedUnixReleaseError.TooLong (256, 255)))

        // The release reaches the guest as single bytes, so anything outside
        // printable ASCII has no faithful encoding — and an embedded NUL would
        // silently truncate what the guest reads.
        create "6.8.0-\u00E9"
        |> shouldEqual (Error (SimulatedUnixReleaseError.NotPrintableAscii (6, '\u00E9')))

        create "6.8.0\u0000bad"
        |> shouldEqual (Error (SimulatedUnixReleaseError.NotPrintableAscii (5, '\u0000')))

        // A forged value bypasses `create` entirely; `assertValid` is what
        // catches it before its null release reaches a guest as `uname -r`.
        let forged =
            Assert.Throws (fun () ->
                SimulatedUnixPlatform.assertValid "test" Unchecked.defaultof<SimulatedUnixPlatform>
                |> ignore<SimulatedUnixPlatform>
            )

        forged.Message |> shouldContainText "Unchecked.defaultof"
