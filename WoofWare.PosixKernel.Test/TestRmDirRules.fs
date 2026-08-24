namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `rmdir(2)`'s per-flavour orderings, row by row against the measurement.
///
/// The host oracle (`TestVirtualFileSystemAgainstHost`) falsifies one column per
/// machine and cannot reach either kernel's root-navigation arms at all: it
/// prefixes every path with a temporary directory, so its stand-in root is an
/// ordinary directory with a real parent while the model's is a genuine
/// filesystem root. `rmdir` is the operation that punishes the difference most —
/// Linux gives `/` EBUSY where Darwin gives EISDIR, and Darwin gives `/.` EBUSY
/// where Linux gives EINVAL — so those rows are pinned here, where the model's
/// root really is one.
///
/// Every row below is measured on macOS 26.6/APFS at uid 501, and Linux 6.x
/// arm64 at uid 1000 and uid 0, one fresh tree per row.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRmDirRules =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

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
    ///   d/           an empty directory: the one thing `rmdir` can remove
    ///   dfull/x      a directory that is not empty
    ///   nest/inner/  two levels, so a `..` can land below the root
    ///   f            a regular file
    ///   lf -> f, ld -> d, lfull -> dfull, dang -> nx, cyc -> cyc
    ///   lroot -> "/", lcur -> ".", lpar -> "nest/inner/..", nest/lcur -> "."
    ///   nowrite/     0o555: searchable, not writable
    ///     kdir/      an empty directory inside it
    ///     kfull/x    a non-empty one
    ///     kid        a file inside it
    ///     klink -> kid
    ///   nosearch/    0o666: writable, not searchable
    ///     kdir/
    ///
    /// The three symlinks whose targets are *navigations* are what make
    /// `FinalNavigation` observable: with Darwin's `Demand` walk, "lcur/" lands
    /// on `Current` and "lpar/" on `Parent`, and the two earn different errnos.
    let private tree : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let dir (parent : InodeNumber) (n : string) (bits : PermissionBits) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createDirectory parent (name n) bits buildTime vfs |> ok

        let file (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createFile parent (name n) PermissionBits.defaultForRegularFile buildTime noBytes vfs
            |> ok
            |> snd

        let link (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createSymlink parent (name n) buildTime (target t) vfs
            |> ok
            |> snd

        let vfs = dir root "d" PermissionBits.defaultForDirectory vfs |> snd

        // Two levels, so that a `..` can land on a directory that is *not* the
        // root. Without it every `..` in this corpus reaches the root, and
        // Darwin's EBUSY arm would be indistinguishable from "`..` is EBUSY".
        let nest, vfs = dir root "nest" PermissionBits.defaultForDirectory vfs
        let vfs = dir nest "inner" PermissionBits.defaultForDirectory vfs |> snd
        // Inside `nest` rather than at the root, so that following it reaches a
        // directory that is not the root: at the root, Darwin's EBUSY arm would
        // swallow the EINVAL this row is for. `inner` stays empty, since two
        // other rows remove it.
        let vfs = link nest "lcur" "." vfs

        let dfull, vfs = dir root "dfull" PermissionBits.defaultForDirectory vfs
        let vfs = file dfull "x" vfs

        let vfs = file root "f" vfs
        let vfs = link root "lf" "f" vfs
        let vfs = link root "ld" "d" vfs
        let vfs = link root "lfull" "dfull" vfs
        let vfs = link root "dang" "nx" vfs
        let vfs = link root "cyc" "cyc" vfs
        let vfs = link root "lroot" "/" vfs
        let vfs = link root "lcur" "." vfs
        let vfs = link root "lpar" "nest/inner/.." vfs

        // The narrowed directories are created *with* their modes: this module
        // has no `chmod`, and a builder applies no permission rule of its own.
        let nowrite, vfs = dir root "nowrite" (mode 0o555) vfs
        let vfs = dir nowrite "kdir" PermissionBits.defaultForDirectory vfs |> snd
        let kfull, vfs = dir nowrite "kfull" PermissionBits.defaultForDirectory vfs
        let vfs = file kfull "x" vfs
        let vfs = file nowrite "kid" vfs
        let vfs = link nowrite "klink" "kid" vfs

        let nosearch, vfs = dir root "nosearch" (mode 0o666) vfs
        let vfs = dir nosearch "kdir" PermissionBits.defaultForDirectory vfs |> snd

        vfs

    /// Resolve as an `rmdir` of the given flavour would, then ask for the
    /// verdict — so the `Resolution` under test is one the walk really produces
    /// rather than one this test hand-assembled. Mirrors what
    /// `SystemNative_RmDir` does, which is why a wrong policy here would be a
    /// wrong policy there too.
    ///
    /// `privilege` reaches the walk as well as the verdict, because the two
    /// split the permission rule between them: the walk refuses a directory this
    /// caller may not search, and the verdict a directory it may not write.
    let private verdict
        (platform : SimulatedUnixPlatform)
        (privilege : CallerPrivilege)
        (candidate : string)
        : RmDirVerdict
        =
        let rules = SimulatedUnixPlatform.rmDirRules platform

        match
            VirtualFileSystem.resolveFull
                (SimulatedUnixPlatform.pathLimits platform)
                privilege
                (VirtualFileSystem.root tree)
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                (path candidate)
                tree
        with
        | Error error -> RmDirVerdict.Refuse error
        | Ok resolution -> RmDirRules.verdict (SimulatedUnixPlatform.flavour platform) privilege resolution tree

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    /// The name a verdict removes, so a row can say *what* would go rather than
    /// only that something would. `rmdir("ld/")` is exactly the row that needs
    /// it: on Darwin the name removed is `d`, not `ld`.
    let private removed (verdict : RmDirVerdict) : string =
        match verdict with
        | RmDirVerdict.Remove (_, name) -> FileName.toString name
        | RmDirVerdict.Refuse error -> failwith $"expected a removal, got %O{error}"

    let private refuses (platform : SimulatedUnixPlatform) (error : UnixError) (candidates : string list) : unit =
        for candidate in candidates do
            let actual = verdict platform CallerPrivilege.Unprivileged candidate

            if actual <> RmDirVerdict.Refuse error then
                failwith $"rmdir(%s{candidate}): expected %O{error}, got %A{actual}"

    // ------------------------------------------------ what both kernels agree

    [<Test>]
    let ``an empty directory is removed by its own name on both kernels`` () : unit =
        for platform in [ linux ; darwin ] do
            for candidate, expected in [ "d", "d" ; "d/", "d" ; "nest/inner", "inner" ; "nest/inner/", "inner" ] do
                removed (verdict platform CallerPrivilege.Unprivileged candidate)
                |> shouldEqual expected

    [<Test>]
    let ``a directory that still holds an entry is ENOTEMPTY on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTEMPTY [ "dfull" ; "dfull/" ]

    [<Test>]
    let ``a free final name is ENOENT and beats the write check`` () : unit =
        // The ordering claim, not just the errno: `nowrite` is unwritable, so a
        // verdict that asked about the parent first would answer EACCES here.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOENT [ "nx" ; "nx/" ; "nowrite/nx" ; "nowrite/nx/" ]

    [<Test>]
    let ``a name inside an unsearchable directory is EACCES on both`` () : unit =
        // Refused by the walk rather than by the verdict, which is why it agrees
        // across flavours where the *write* rule does not.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nosearch/kdir" ; "nosearch/kdir/" ]

    [<Test>]
    let ``removing an empty directory from an unwritable parent is EACCES on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nowrite/kdir" ; "nowrite/kdir/" ]

    [<Test>]
    let ``the write check beats the emptiness check on both`` () : unit =
        // `nowrite/kfull` is both unwritable-from and non-empty. Measured EACCES
        // on each, so neither flavour reaches ENOTEMPTY first.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nowrite/kfull" ; "nowrite/kfull/" ]

    [<Test>]
    let ``a symlink named without a separator is ENOTDIR on both`` () : unit =
        // `NoFollowFinal`, so the final name resolves to the link itself, which
        // is not a directory whatever it points at.
        for platform in [ linux ; darwin ] do
            refuses
                platform
                UnixError.ENOTDIR
                [
                    "lf"
                    "ld"
                    "lfull"
                    "dang"
                    "cyc"
                    "lroot"
                    "lcur"
                    "lpar"
                    "nest/lcur"
                ]

    [<Test>]
    let ``a regular file is ENOTDIR on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTDIR [ "f" ; "f/" ]

    [<Test>]
    let ``a path ending in "." below the root is EINVAL on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EINVAL [ "d/." ; "nest/inner/." ]

    [<Test>]
    let ``a path ending in ".." below the root is ENOTEMPTY on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTEMPTY [ "nest/inner/.." ]

    [<Test>]
    let ``the navigation arms beat the write check on both`` () : unit =
        // The rows that make `FinalNavigation` a separate arm rather than a
        // coincidence: `nowrite/kdir/.` reaches an unwritable directory, and
        // both kernels answer the navigation's errno rather than EACCES.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EINVAL [ "nowrite/kdir/." ]
            refuses platform UnixError.ENOTEMPTY [ "nowrite/kdir/.." ]

    [<Test>]
    let ``an over-long component is ENAMETOOLONG on both`` () : unit =
        let long = System.String ('a', 300)

        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENAMETOOLONG [ long ; long + "/" ]

    // ---------------------------------------------------------------- Linux

    [<Test>]
    let ``Linux gives the root itself EBUSY`` () : unit =
        // A path that consumed no component at all. Darwin answers EISDIR, so
        // this row is one of the two that make the root arms per-flavour.
        refuses linux UnixError.EBUSY [ "/" ]

    [<Test>]
    let ``Linux specialises the path rather than the root inode`` () : unit =
        // `/.` and `/..` reach the root, and Linux gives them exactly what it
        // gives `.` and `..` on any other directory. Darwin gives both EBUSY.
        refuses linux UnixError.EINVAL [ "." ; "./" ; "/." ; "lroot/." ]
        refuses linux UnixError.ENOTEMPTY [ ".." ; "d/.." ; "/.." ; "lroot/.." ]

    [<Test>]
    let ``Linux never traverses a final symlink for a trailing separator`` () : unit =
        // `TrailingSeparatorPolicy.Ignore`: the final component is never
        // resolved, so every one of these is "the name I was given is not a
        // directory". `ld/` is the destructive row — Darwin removes `d` here.
        refuses
            linux
            UnixError.ENOTDIR
            [
                "ld/"
                "lf/"
                "lfull/"
                "dang/"
                "cyc/"
                "lroot/"
                "lcur/"
                "lpar/"
                "nest/lcur/"
            ]

    [<Test>]
    let ``Linux checks the parent's write bit before the target's type`` () : unit =
        // `nowrite/kid` is a *file* inside a directory this caller cannot write.
        // Linux answers EACCES, Darwin ENOTDIR. Measured at uid 0, Linux answers
        // ENOTDIR — which is the same ordering seen from the other side.
        refuses linux UnixError.EACCES [ "nowrite/kid" ; "nowrite/kid/" ; "nowrite/klink" ; "nowrite/klink/" ]

        verdict linux CallerPrivilege.Privileged "nowrite/kid"
        |> shouldEqual (RmDirVerdict.Refuse UnixError.ENOTDIR)

    // --------------------------------------------------------------- Darwin

    [<Test>]
    let ``Darwin gives the root itself EISDIR`` () : unit =
        // Reached directly, and — because Darwin's walk follows a final symlink
        // under a trailing separator — through `lroot -> "/"`, which Linux
        // answers ENOTDIR.
        refuses darwin UnixError.EISDIR [ "/" ; "lroot/" ]

    [<Test>]
    let ``Darwin specialises the root inode rather than the path`` () : unit =
        // XNU refuses a mount's root vnode before it looks at which navigation
        // got there; PawPrint mounts one filesystem, so that is the root. Below
        // the root the two flavours agree again, which is what `nest/inner`
        // pins.
        // "d/.." is in the EBUSY list rather than the ENOTEMPTY one because `d`
        // is a child of *this* root, so climbing out of it reaches the root.
        // That is why the corpus carries `nest/inner`: only a directory two
        // levels down can show that Darwin agrees with Linux below the root.
        refuses darwin UnixError.EBUSY [ "/." ; "/.." ; "lroot/." ; "lroot/.." ; "." ; "./" ; ".." ; "d/.." ; "lcur/" ]
        refuses darwin UnixError.EINVAL [ "d/." ; "nest/inner/." ]
        refuses darwin UnixError.ENOTEMPTY [ "nest/inner/.." ]

    [<Test>]
    let ``Darwin traverses a final symlink for a trailing separator`` () : unit =
        // `TrailingSeparatorPolicy.Demand`, so the walk resolves the link and
        // then imposes the demand — which is how each of these lands on what the
        // link *named* rather than on the link.
        refuses darwin UnixError.ENOTDIR [ "lf/" ]
        refuses darwin UnixError.ENOTEMPTY [ "lfull/" ; "lpar/" ]
        refuses darwin UnixError.EINVAL [ "nest/lcur/" ]
        refuses darwin UnixError.ENOENT [ "dang/" ]
        refuses darwin UnixError.ELOOP [ "cyc/" ]

    [<Test>]
    let ``Darwin removes the target of a final symlink`` () : unit =
        // The destructive divergence, and the reason this syscall dispatches on
        // the flavour instead of picking a column: with `ld -> d`, Darwin's
        // `rmdir("ld/")` removes `d` while Linux's is ENOTDIR. A handler that
        // hardcoded either would delete the wrong object on the other platform.
        removed (verdict darwin CallerPrivilege.Unprivileged "ld/") |> shouldEqual "d"

    [<Test>]
    let ``Darwin checks the target's type before the parent's write bit`` () : unit =
        // The mirror of the Linux row: `nowrite/kid` is a file in a directory
        // this caller cannot write, and Darwin reports the type.
        refuses darwin UnixError.ENOTDIR [ "nowrite/kid" ; "nowrite/kid/" ; "nowrite/klink" ; "nowrite/klink/" ]

    // ------------------------------------------------------------ privilege

    [<Test>]
    let ``privilege gates the write bit and nothing else`` () : unit =
        // Measured at uid 0 on Linux, every row: the EACCES rows fall through to
        // their next check and no other arm moves. So `CallerPrivilege` must
        // reach exactly one function.
        for platform in [ linux ; darwin ] do
            removed (verdict platform CallerPrivilege.Privileged "nowrite/kdir")
            |> shouldEqual "kdir"

            verdict platform CallerPrivilege.Privileged "nowrite/kfull"
            |> shouldEqual (RmDirVerdict.Refuse UnixError.ENOTEMPTY)

            // Unmoved by privilege: the navigation arms, the type check and the
            // emptiness check.
            verdict platform CallerPrivilege.Privileged "dfull"
            |> shouldEqual (RmDirVerdict.Refuse UnixError.ENOTEMPTY)

            verdict platform CallerPrivilege.Privileged "f"
            |> shouldEqual (RmDirVerdict.Refuse UnixError.ENOTDIR)

            verdict platform CallerPrivilege.Privileged "nest/inner/."
            |> shouldEqual (RmDirVerdict.Refuse UnixError.EINVAL)

    // ------------------------------------------------------------ the rules

    [<Test>]
    let ``the flavours disagree about the removed directory's own ctime`` () : unit =
        // Measured through a descriptor held across the call, reproduced 3/3 on
        // each: Linux drops `st_nlink` 2 -> 0 and moves the directory's `ctime`,
        // Darwin leaves both alone. Guest-observable through `fstat` on a
        // directory descriptor.
        (SimulatedUnixPlatform.rmDirRules linux).RemovedDirectoryEffect
        |> shouldEqual UnbindTargetEffect.LostALink

        (SimulatedUnixPlatform.rmDirRules darwin).RemovedDirectoryEffect
        |> shouldEqual UnbindTargetEffect.Untouched
