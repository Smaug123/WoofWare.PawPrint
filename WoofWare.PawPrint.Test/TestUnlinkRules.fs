namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `unlink(2)`'s per-flavour orderings, row by row against the measurement.
///
/// The host oracle (`TestVirtualFileSystemAgainstHost`) falsifies one column per
/// machine and cannot reach either kernel's root-navigation arms at all: it
/// prefixes every path with a temporary directory, so its stand-in root is an
/// ordinary directory with a real parent while the model's is a genuine
/// filesystem root, and deletion is the first operation whose answer depends on
/// the difference. Those rows are pinned here, where the model's root really is
/// one.
///
/// Every row below is measured on macOS 26.6/APFS at uid 501 and 0, and Linux 6.x
/// arm64 at uid 1000 and 0, one fresh tree per row.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnlinkRules =

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
    ///   d/           an empty directory
    ///   dfull/x      a directory that is not empty
    ///   nest/inner/  two levels, so a `..` can land below the root
    ///   f, g         regular files
    ///   lf -> f, ld -> d, dang -> nx, cyc -> cyc, lroot -> "/"
    ///   nowrite/     0o555: searchable, not writable
    ///     kdir/      a directory inside it
    ///     kid        a file inside it
    ///     klink -> kid
    ///   nosearch/    0o666: writable, not searchable
    ///     kid
    ///
    /// `nowrite` is what separates the two flavours' orderings: Linux answers
    /// EACCES for `nowrite/kdir` where Darwin answers EPERM, because the two
    /// order the write check and the is-a-directory check the opposite way
    /// round.
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

        let dfull, vfs = dir root "dfull" PermissionBits.defaultForDirectory vfs
        let vfs = file dfull "x" vfs

        let vfs = file root "f" vfs
        let vfs = file root "g" vfs
        let vfs = link root "lf" "f" vfs
        let vfs = link root "ld" "d" vfs
        let vfs = link root "dang" "nx" vfs
        let vfs = link root "cyc" "cyc" vfs
        let vfs = link root "lroot" "/" vfs

        // The narrowed directories are created *with* their modes: this module
        // has no `chmod`, and a builder applies no permission rule of its own.
        let nowrite, vfs = dir root "nowrite" (mode 0o555) vfs
        let vfs = dir nowrite "kdir" PermissionBits.defaultForDirectory vfs |> snd
        let vfs = file nowrite "kid" vfs
        let vfs = link nowrite "klink" "kid" vfs

        let nosearch, vfs = dir root "nosearch" (mode 0o666) vfs
        let vfs = file nosearch "kid" vfs

        vfs

    /// Resolve as an `unlink` of the given flavour would, then ask for the
    /// verdict — so the `Resolution` under test is one the walk really produces
    /// rather than one this test hand-assembled. Mirrors what
    /// `SystemNative_Unlink` does, which is why a wrong policy here would be a
    /// wrong policy there too.
    ///
    /// `privilege` reaches the walk as well as the verdict, because the two
    /// split the permission rule between them: the walk refuses a directory this
    /// caller may not search, and the verdict a directory it may not write.
    let private verdict
        (platform : SimulatedUnixPlatform)
        (privilege : CallerPrivilege)
        (candidate : string)
        : UnlinkVerdict
        =
        let rules = SimulatedUnixPlatform.unlinkRules platform

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
        | Error error -> UnlinkVerdict.Refuse error
        | Ok resolution -> UnlinkRules.verdict (SimulatedUnixPlatform.flavour platform) privilege resolution tree

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    /// The name a verdict removes, so a row can say *what* would go rather than
    /// only that something would.
    let private removed (verdict : UnlinkVerdict) : string =
        match verdict with
        | UnlinkVerdict.Remove (_, name) -> FileName.toString name
        | UnlinkVerdict.Refuse error -> failwith $"expected a removal, got %O{error}"

    let private refuses (platform : SimulatedUnixPlatform) (error : UnixError) (candidates : string list) : unit =
        for candidate in candidates do
            let actual = verdict platform CallerPrivilege.Unprivileged candidate

            if actual <> UnlinkVerdict.Refuse error then
                failwith $"unlink(%s{candidate}): expected %O{error}, got %A{actual}"

    // ------------------------------------------------ what both kernels agree

    [<Test>]
    let ``a removable name is removed by its own name on both kernels`` () : unit =
        // Including every symlink shape: `unlink` removes the *link*, never what
        // it points at, so a dangling link and a cyclic one go as readily as a
        // link to a real file, and `lroot -> "/"` goes without the root being
        // consulted at all.
        for platform in [ linux ; darwin ] do
            for candidate in [ "f" ; "g" ; "lf" ; "ld" ; "dang" ; "cyc" ; "lroot" ] do
                removed (verdict platform CallerPrivilege.Unprivileged candidate)
                |> shouldEqual candidate

    [<Test>]
    let ``a free final name is ENOENT and beats every check below it`` () : unit =
        // The ordering claim, not just the errno: `nowrite` is unwritable and
        // `nowrite/nx/` carries a trailing separator, so a verdict that checked
        // either first would answer EACCES or ENOTDIR here.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOENT [ "nx" ; "nowrite/nx" ; "nowrite/nx/" ]

    [<Test>]
    let ``a name inside an unsearchable directory is EACCES on both`` () : unit =
        // Refused by the walk rather than by the verdict, which is why it agrees
        // across flavours where the *write* rule below does not.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nosearch/kid" ; "nosearch/kid/" ]

    [<Test>]
    let ``removing a file from an unwritable directory is EACCES on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nowrite/kid" ; "nowrite/klink" ]

    [<Test>]
    let ``a trailing separator over a non-directory is ENOTDIR on both`` () : unit =
        // Reached by opposite routes: Darwin's walk enforces the demand, while
        // Linux's ignores it and `linuxVerdict` enforces it. That the answers
        // agree here is what makes the `lroot/` row below the one that separates
        // them.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTDIR [ "f/" ; "lf/" ; "nowrite/kid/" ; "nowrite/klink/" ]

    [<Test>]
    let ``the root itself is EISDIR on both`` () : unit =
        // A path that consumed no component at all. Unreachable from the host
        // oracle, whose "/" is the machine's root rather than the corpus's.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EISDIR [ "/" ]

    [<Test>]
    let ``an over-long component is ENAMETOOLONG before anything else`` () : unit =
        // The length check is the walk's and beats the trailing-separator answer
        // on both, which is the opposite of `TrailingSeparatorPolicy.RefuseIsDirectory`
        // — a second, independent confirmation that deletion's trailing
        // enforcement is not a walk-level refusal.
        let long = String.replicate 300 "a"

        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENAMETOOLONG [ long ; long + "/" ]

    [<Test>]
    let ``privilege gates the write bit and nothing else`` () : unit =
        // Measured at uid 0 on both: `unlink` of a directory is still refused
        // (EISDIR on Linux, EPERM on Darwin), while the unwritable parent stops
        // mattering. The `unlink(2)` man page's "and the effective user ID of the
        // process is not the super-user" is stale relative to modern XNU.
        removed (verdict linux CallerPrivilege.Privileged "nowrite/kid")
        |> shouldEqual "kid"

        removed (verdict darwin CallerPrivilege.Privileged "nowrite/kid")
        |> shouldEqual "kid"

        verdict linux CallerPrivilege.Privileged "d"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EISDIR)

        verdict darwin CallerPrivilege.Privileged "d"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EPERM)

        verdict linux CallerPrivilege.Privileged "nowrite/kdir"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EISDIR)

        verdict darwin CallerPrivilege.Privileged "nowrite/kdir"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EPERM)

    // --------------------------------------------------------------- Linux

    [<Test>]
    let ``Linux answers EISDIR for every directory, however it was reached`` () : unit =
        // Linux spends no errno distinguishing the navigations, where `rmdir`
        // owes all three different ones and Darwin's `unlink` owes two.
        refuses
            linux
            UnixError.EISDIR
            [
                "d"
                "dfull"
                "."
                ".."
                "./"
                "d/."
                "d/.."
                "nest/inner/."
                "nest/inner/.."
                "/."
                "/.."
                "lroot/."
                "lroot/.."
                // With a trailing separator, from the verdict's own demand arm.
                "d/"
                "nowrite/kdir/"
            ]

    [<Test>]
    let ``Linux never traverses a final symlink for a trailing separator`` () : unit =
        // `TrailingSeparatorPolicy.Ignore`: no traversal, so no ELOOP for the
        // cycle, no ENOENT for the dangling link, and — the row that proves it —
        // ENOTDIR rather than EISDIR for a link whose target is the root.
        refuses linux UnixError.ENOTDIR [ "ld/" ; "dang/" ; "cyc/" ; "lroot/" ]

    [<Test>]
    let ``Linux checks the write bit before the target's type`` () : unit =
        // The pair that pins the order, and the only thing separating
        // `linuxVerdict`'s two EISDIR arms: without the separator the unwritable
        // parent wins, with it the directory demand does.
        verdict linux CallerPrivilege.Unprivileged "nowrite/kdir"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EACCES)

        verdict linux CallerPrivilege.Unprivileged "nowrite/kdir/"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EISDIR)

    // --------------------------------------------------------------- Darwin

    [<Test>]
    let ``Darwin answers EBUSY for the root reached by a navigation`` () : unit =
        // XNU's `unlink1` refuses a mount's root vnode (`vp->v_flag & VROOT`);
        // PawPrint mounts one filesystem, so that is the root. Reached through a
        // symlink to "/" as readily as directly.
        // `d/..` is here rather than beside `d/.` for exactly that reason: this
        // corpus's root *is* the filesystem root, so climbing out of `d` lands
        // on it.
        refuses darwin UnixError.EBUSY [ "/." ; "/.." ; "lroot/." ; "lroot/.." ; "d/.." ; "." ; ".." ]

    [<Test>]
    let ``Darwin answers EPERM for any other directory`` () : unit =
        // The contrast with the row above: `d` is not a mount root, so it earns
        // the ordinary directory refusal whichever way it was reached.
        refuses
            darwin
            UnixError.EPERM
            [
                "d"
                "dfull"
                "d/"
                "nowrite/kdir"
                "nowrite/kdir/"
                // Reached by a navigation, which is what separates this arm
                // from the EBUSY one above: same shape of path, ordinary
                // directory at the end of it.
                "d/."
                "nest/inner/."
                "nest/inner/.."
            ]

    [<Test>]
    let ``Darwin traverses a final symlink for a trailing separator`` () : unit =
        // `TrailingSeparatorPolicy.Demand`, which is where the two flavours part:
        // every one of these is ENOTDIR on Linux.
        verdict darwin CallerPrivilege.Unprivileged "dang/"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.ENOENT)

        verdict darwin CallerPrivilege.Unprivileged "cyc/"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.ELOOP)

        verdict darwin CallerPrivilege.Unprivileged "lroot/"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EISDIR)

        // The destructive pair `Resolution.FinalSymlinkFollowed` warns about:
        // the walk followed `ld` to the directory it names, and the verdict
        // refuses it. Nothing is destroyed on either flavour, which is why
        // `unlink` needs no loud failure where `rmdir` will.
        verdict darwin CallerPrivilege.Unprivileged "ld/"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EPERM)

    [<Test>]
    let ``Darwin checks the target's type before the write bit`` () : unit =
        // The mirror image of the Linux pair: here the directory wins without
        // the separator too.
        verdict darwin CallerPrivilege.Unprivileged "nowrite/kdir"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EPERM)

        verdict darwin CallerPrivilege.Unprivileged "nowrite/kid"
        |> shouldEqual (UnlinkVerdict.Refuse UnixError.EACCES)
