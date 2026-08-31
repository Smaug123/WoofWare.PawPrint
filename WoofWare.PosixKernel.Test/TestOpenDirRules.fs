namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `opendir(3)`'s verdict, row by row against the measurement.
///
/// Every row here was measured on **both** kernels — macOS 26.6/APFS at uid 501
/// and Linux 6.x arm64 at uid 1000 — and they agreed on every one, which is why
/// `OpenDirRules.verdict` takes no flavour. The fixture still drives both
/// platforms, because "they agree" is the claim being made: a rule that
/// consulted the flavour would have to be wrong on one of them to pass.
///
/// The host oracle (`TestVirtualFileSystemAgainstHost`) falsifies one column per
/// machine, and there are two things it structurally cannot reach. It prefixes
/// every path with a temporary directory, so it can never ask about the real
/// root; and it runs at whatever privilege the suite happens to have, so it
/// cannot ask what root sees. Both are pinned here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpenDirRules =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private path (s : string) : UnixPath = UnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    let private mode (raw : int) : PermissionBits = PermissionBits.parseOrFail "test" raw

    let private ok (result : Result<'a, UnixError>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"could not build the tree: %O{error}"

    /// The corpus, mirroring the probe that measured these rows.
    ///
    ///   d/x          an ordinary readable directory
    ///   f            a regular file, mode 0o644
    ///   f0           a regular file, mode 0o000
    ///   lf -> f, lf0 -> f0, ld -> d, dang -> nx
    ///   dr/x         0o111: searchable, not readable
    ///   ldr -> dr
    ///   dw/x         0o444: readable, not searchable
    ///   d0/x         0o000
    ///   nosearch/    0o666: writable, not searchable, holding `kdir`
    ///
    /// The asymmetric pair is `dr` (search, no read) and `dw` (read, no search):
    /// without both, a verdict that demanded the *search* bit instead of the read
    /// bit would answer every row correctly.
    let private tree : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let dir (parent : InodeNumber) (n : string) (bits : PermissionBits) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createDirectory parent (name n) bits buildTime vfs |> ok

        let file (parent : InodeNumber) (n : string) (bits : PermissionBits) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createFile parent (name n) bits buildTime noBytes vfs
            |> ok
            |> snd

        let link (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createSymlink parent (name n) buildTime (target t) vfs
            |> ok
            |> snd

        let d, vfs = dir root "d" SeedEntry.defaultPermsForDirectory vfs
        let vfs = file d "x" SeedEntry.defaultPermsForRegularFile vfs

        let vfs = file root "f" SeedEntry.defaultPermsForRegularFile vfs
        let vfs = file root "f0" (mode 0o000) vfs
        let vfs = link root "lf" "f" vfs
        let vfs = link root "lf0" "f0" vfs
        let vfs = link root "ld" "d" vfs
        let vfs = link root "dang" "nx" vfs

        // The narrowed directories are created *with* their modes: this module
        // has no `chmod`, and a builder applies no permission rule of its own.
        let dr, vfs = dir root "dr" (mode 0o111) vfs
        let vfs = file dr "x" SeedEntry.defaultPermsForRegularFile vfs
        let vfs = link root "ldr" "dr" vfs

        let dw, vfs = dir root "dw" (mode 0o444) vfs
        let vfs = file dw "x" SeedEntry.defaultPermsForRegularFile vfs

        let d0, vfs = dir root "d0" (mode 0o000) vfs
        let vfs = file d0 "x" SeedEntry.defaultPermsForRegularFile vfs

        let nosearch, vfs = dir root "nosearch" (mode 0o666) vfs
        let vfs = dir nosearch "kdir" SeedEntry.defaultPermsForDirectory vfs |> snd

        vfs

    /// Resolve as `opendir` does, then ask for the verdict — so the `Resolution`
    /// under test is one the walk really produces rather than one this test
    /// hand-assembled. Mirrors what `SystemNative_OpenDir` does, which is why a
    /// wrong policy here would be a wrong policy there too.
    ///
    /// `privilege` reaches the walk as well as the verdict, because the two
    /// split the permission rule between them: the walk refuses a directory this
    /// caller may not search, and the verdict one it may not read.
    let private verdict
        (platform : SimulatedUnixPlatform)
        (privilege : CallerPrivilege)
        (candidate : string)
        : OpenDirVerdict
        =
        match
            PathWalk.resolveFull
                (SimulatedUnixPlatform.pathLimits platform)
                privilege
                (VirtualFileSystem.root tree)
                SymlinkPolicy.Follow
                TrailingSeparatorPolicy.Demand
                (path candidate)
                tree
        with
        | Error error -> OpenDirVerdict.Refuse error
        | Ok resolution -> OpenDirRules.verdict privilege resolution tree

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    let private platforms = [ linux ; darwin ]

    let private refuses (error : UnixError) (candidates : string list) : unit =
        for platform in platforms do
            for candidate in candidates do
                let actual = verdict platform CallerPrivilege.Unprivileged candidate

                if actual <> OpenDirVerdict.Refuse error then
                    failwith $"opendir(%s{candidate}) on %O{platform}: expected %O{error}, got %A{actual}"

    let private opens (candidates : string list) : unit =
        for platform in platforms do
            for candidate in candidates do
                match verdict platform CallerPrivilege.Unprivileged candidate with
                | OpenDirVerdict.Open _ -> ()
                | OpenDirVerdict.Refuse error ->
                    failwith $"opendir(%s{candidate}) on %O{platform}: expected success, got %O{error}"

    // ------------------------------------------------------------ what opens

    [<Test>]
    let ``an ordinary directory opens, with or without a trailing separator`` () : unit = opens [ "d" ; "d/" ; "/d" ]

    [<Test>]
    let ``the walk follows a final symlink to a directory`` () : unit = opens [ "ld" ; "ld/" ]

    [<Test>]
    let ``the root and the navigation names open`` () : unit =
        // The rows the host oracle structurally cannot reach: its stand-in root
        // is an ordinary directory with a real parent. `rmdir`'s three
        // root-navigation arms are the reason to state this rather than leave it
        // implied — `opendir` specialises none of them.
        opens [ "/" ; "d/." ; "d/.." ; "/." ; "/.." ]

    [<Test>]
    let ``a readable but unsearchable directory opens and is the asymmetric half`` () : unit =
        // 0o444: read, no search. Together with `dr` below this is what pins the
        // verdict to the *read* bit. A rule demanding search would refuse this
        // and admit `dr`, and every other row in this file would still pass.
        opens [ "dw" ; "dw/" ]

    // ---------------------------------------------------------- what refuses

    [<Test>]
    let ``a name nothing binds is ENOENT`` () : unit =
        refuses UnixError.ENOENT [ "nx" ; "d/nx" ]

    [<Test>]
    let ``a dangling symlink is ENOENT, because the walk followed it`` () : unit =
        refuses UnixError.ENOENT [ "dang" ; "dang/" ]

    [<Test>]
    let ``a regular file is ENOTDIR, through a link and with a separator`` () : unit =
        refuses UnixError.ENOTDIR [ "f" ; "f/" ; "lf" ; "lf/" ]

    [<Test>]
    let ``a searchable but unreadable directory is EACCES`` () : unit =
        // 0o111: search, no read. The other half of the asymmetric pair.
        refuses UnixError.EACCES [ "dr" ; "dr/" ; "ldr" ; "ldr/" ; "d0" ]

    [<Test>]
    let ``an unsearchable parent refuses before the name is looked up`` () : unit =
        // The walk's rule rather than the verdict's, and it beats ENOENT:
        // measured on both, `opendir("nosearch/nx")` is EACCES rather than
        // ENOENT even though nothing binds that name.
        refuses UnixError.EACCES [ "nosearch/kdir" ; "nosearch/nx" ]

    // ------------------------------------------------------------- the order

    [<Test>]
    let ``being a file beats being unreadable`` () : unit =
        // The row that pins the arm order: a **mode-0000** regular file is
        // ENOTDIR and not EACCES, measured on both kernels, with and without a
        // trailing separator and through a symlink to one. Swapping the two arms
        // in `OpenDirRules.verdict` makes exactly this test fail and nothing
        // else — the file rows above are all mode 0o644, where a permission
        // check would pass anyway.
        refuses UnixError.ENOTDIR [ "f0" ; "f0/" ; "lf0" ; "lf0/" ]

    // --------------------------------------------------------- and for root

    [<Test>]
    let ``root reads a directory whatever its mode says`` () : unit =
        // The other thing the host oracle cannot reach: it runs at whatever
        // privilege the suite has. Measured on Linux as uid 0.
        for platform in platforms do
            for candidate in [ "dr" ; "d0" ; "d" ] do
                match verdict platform CallerPrivilege.Privileged candidate with
                | OpenDirVerdict.Open _ -> ()
                | OpenDirVerdict.Refuse error ->
                    failwith $"opendir(%s{candidate}) as root on %O{platform}: expected success, got %O{error}"

    [<Test>]
    let ``root is still refused a file`` () : unit =
        // Privilege bypasses the mode, not the type: ENOTDIR is not a permission
        // failure and no uid escapes it.
        for platform in platforms do
            verdict platform CallerPrivilege.Privileged "f0"
            |> shouldEqual (OpenDirVerdict.Refuse UnixError.ENOTDIR)
