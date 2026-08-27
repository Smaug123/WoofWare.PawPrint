namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `rename(2)`'s per-flavour orderings, row by row against the measurement.
///
/// Every row below is measured on macOS 26.6/APFS at uid 501, and Linux 6.18
/// arm64 (ext4 on a real block device, not the container's overlay) at uid 1000
/// and uid 0, one fresh tree per row. `docs/probes/rename/` holds the probes
/// that took them, and is the thing to re-run rather than to re-derive.
///
/// Two kinds of row cannot live in `TestVirtualFileSystemAgainstHost` and so are
/// pinned here. The host oracle prefixes every path with a temporary directory,
/// so its stand-in root is an ordinary directory with a real parent, where the
/// model's is a genuine filesystem root — which is exactly what the six
/// navigation arms are about. And it runs on one machine, so it falsifies one
/// flavour's column and is silent about the other's.
///
/// **Not** in scope here: which of the two paths is resolved first when both are
/// bad. That is `RenameRules.WalkOrder`'s business, it diverges too, and it is
/// its own slice — the driver below resolves the source and then the
/// destination, which is Darwin's order, so no row here pairs two failing
/// resolutions.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRenameRules =

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

    /// The corpus, mirroring the probes that measured these rows.
    ///
    ///   f, g         a regular file and a hard link to it — the same-inode no-op
    ///   d, d2        two empty directories
    ///   dfull/kid    a directory that is not empty
    ///   lf -> f, ld -> d, dang -> nx, lroot -> "/"
    ///   a/b/c, a/b/file   three levels plus a file, for the subtree rows
    ///   ab           a name with `a` as a string prefix but not as an ancestor
    ///   mv           a directory at 0o555, for the moved-directory ".." rewrite
    ///   p/           0o555: searchable, not writable
    ///     pf, ph     a file and a hard link to it, both inside p
    ///     pd, pm     two empty directories inside p
    ///   q/           0o555: searchable, not writable
    ///     qd         an empty directory, qfull/kid a non-empty one, qfile a file
    ///     lq -> /d2  a symlink to a directory, inside the unwritable parent
    ///   w/           0o755, the writable destination parent
    ///     wf, wd, wfull/kid   a file, an empty directory, a non-empty directory
    ///     wzero, wzerofull/kid  the same two at mode 0o000
    ///   nosearch/kid 0o666: writable, not searchable
    ///
    /// `wzero` is what makes Darwin's strangest arm observable: a directory
    /// displacing a directory consults the *displaced* directory's write bit
    /// there, and its parent's on Linux.
    let private tree : VirtualFileSystem =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let dir (parent : InodeNumber) (n : string) (bits : PermissionBits) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createDirectory parent (name n) bits buildTime vfs |> ok

        let file (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createFile parent (name n) PermissionBits.defaultForRegularFile buildTime noBytes vfs
            |> ok

        let link (parent : InodeNumber) (n : string) (t : string) (vfs : VirtualFileSystem) =
            VirtualFileSystem.createSymlink parent (name n) buildTime (target t) vfs
            |> ok
            |> snd

        let plainDir = PermissionBits.defaultForDirectory

        let f, vfs = file root "f" vfs
        let vfs = VirtualFileSystem.hardLink root (name "g") f buildTime vfs |> ok

        let vfs = dir root "d" plainDir vfs |> snd
        let vfs = dir root "d2" plainDir vfs |> snd
        let dfull, vfs = dir root "dfull" plainDir vfs
        let vfs = file dfull "kid" vfs |> snd

        let vfs = link root "lf" "f" vfs
        let vfs = link root "ld" "d" vfs
        let vfs = link root "dang" "nx" vfs
        let vfs = link root "lroot" "/" vfs

        let a, vfs = dir root "a" plainDir vfs
        let b, vfs = dir a "b" plainDir vfs
        let vfs = dir b "c" plainDir vfs |> snd
        let vfs = file b "file" vfs |> snd
        let vfs = dir root "ab" plainDir vfs |> snd

        let vfs = dir root "mv" (mode 0o555) vfs |> snd
        // Somewhere for `mv` to land within its own parent: a free name, an
        // existing empty directory, and an existing non-empty one. Those three
        // are what separate Linux's ".."-rewrite rule from Darwin's wider one.
        let vfs = dir root "mvdest" plainDir vfs |> snd
        let mvfull, vfs = dir root "mvfull" plainDir vfs
        let vfs = file mvfull "kid" vfs |> snd

        let p, vfs = dir root "p" (mode 0o555) vfs
        let pf, vfs = file p "pf" vfs
        let vfs = VirtualFileSystem.hardLink p (name "ph") pf buildTime vfs |> ok
        let vfs = dir p "pd" plainDir vfs |> snd
        let vfs = dir p "pm" plainDir vfs |> snd

        let q, vfs = dir root "q" (mode 0o555) vfs
        let vfs = dir q "qd" plainDir vfs |> snd
        let qfull, vfs = dir q "qfull" plainDir vfs
        let vfs = file qfull "kid" vfs |> snd
        let vfs = file q "qfile" vfs |> snd
        // A symlink to a directory inside the unwritable `q`. It is what makes
        // the destination's trailing-separator arm distinguishable from the type
        // rule below it: with `q` writable the two both say ENOTDIR.
        let vfs = link q "lq" "/d2" vfs

        let w, vfs = dir root "w" plainDir vfs
        let vfs = file w "wf" vfs |> snd
        let vfs = dir w "wd" plainDir vfs |> snd
        let wfull, vfs = dir w "wfull" plainDir vfs
        let vfs = file wfull "kid" vfs |> snd
        let vfs = dir w "wzero" (mode 0o000) vfs |> snd
        let wzerofull, vfs = dir w "wzerofull" (mode 0o000) vfs
        let vfs = file wzerofull "kid" vfs |> snd

        let nosearch, vfs = dir root "nosearch" (mode 0o666) vfs
        let vfs = file nosearch "kid" vfs |> snd

        vfs

    /// A directory that has lost its last name while something still holds it —
    /// the only way a guest reaches one is by having it as its current
    /// directory, which is why the driver below takes a start directory rather
    /// than always walking from the root.
    let private orphanState =
        let vfs = tree
        let root = VirtualFileSystem.root vfs

        let inode, vfs =
            VirtualFileSystem.createDirectory root (name "gone") PermissionBits.defaultForDirectory buildTime vfs
            |> ok

        let unbound, vfs =
            VirtualFileSystem.unbind UnbindTargetEffect.LostALink root (name "gone") buildTime vfs
            |> ok

        unbound |> shouldEqual inode
        VirtualFileSystem.isOrphanedDirectory inode vfs |> shouldEqual true
        vfs, inode

    let private orphanTree : VirtualFileSystem = fst orphanState

    let private orphan : InodeNumber = snd orphanState

    /// Resolve both paths as a `rename` of the given flavour would, then ask for
    /// the verdict — so the two `Resolution`s under test are ones the walk really
    /// produces rather than ones this test hand-assembled. This is what
    /// `SystemNative_Rename` will do, which is why a wrong policy here would be a
    /// wrong policy there.
    ///
    /// `privilege` reaches the walks as well as the verdict, because the two
    /// split the permission rule between them: the walk refuses a directory this
    /// caller may not search, and the verdict the ones it may not write.
    let private verdictIn
        (vfs : VirtualFileSystem)
        (startDirectory : InodeNumber)
        (platform : SimulatedUnixPlatform)
        (privilege : CallerPrivilege)
        (source : string)
        (destination : string)
        : RenameVerdict
        =
        let rules = SimulatedUnixPlatform.renameRules platform

        let resolve (candidate : string) =
            VirtualFileSystem.resolveFull
                (SimulatedUnixPlatform.pathLimits platform)
                privilege
                startDirectory
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                (path candidate)
                vfs

        match resolve source with
        | Error error -> RenameVerdict.Refuse error
        | Ok sourceResolution ->

        match resolve destination with
        | Error error -> RenameVerdict.Refuse error
        | Ok destinationResolution ->

        RenameRules.verdict
            (SimulatedUnixPlatform.flavour platform)
            privilege
            sourceResolution
            destinationResolution
            vfs

    let private verdict
        (platform : SimulatedUnixPlatform)
        (privilege : CallerPrivilege)
        (source : string)
        (destination : string)
        : RenameVerdict
        =
        verdictIn tree (VirtualFileSystem.root tree) platform privilege source destination

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    /// The names a verdict would move between, so a row can say *what* would be
    /// renamed rather than only that something would. The trailing-separator rows
    /// are exactly the ones that need it: on Darwin `rename("ld/", "moved")`
    /// moves `d`, not `ld`.
    let private moves (verdict : RenameVerdict) : string * string =
        match verdict with
        | RenameVerdict.Move (_, sourceName, _, destinationName) ->
            FileName.toString sourceName, FileName.toString destinationName
        | other -> failwith $"expected a move, got %A{other}"

    let private refuses (platform : SimulatedUnixPlatform) (error : UnixError) (rows : (string * string) list) : unit =
        for source, destination in rows do
            let actual = verdict platform CallerPrivilege.Unprivileged source destination

            if actual <> RenameVerdict.Refuse error then
                failwith $"rename(%s{source}, %s{destination}): expected %O{error}, got %A{actual}"

    let private succeeds (platform : SimulatedUnixPlatform) (rows : (string * string) list) : unit =
        for source, destination in rows do
            match verdict platform CallerPrivilege.Unprivileged source destination with
            | RenameVerdict.Move _ -> ()
            | other -> failwith $"rename(%s{source}, %s{destination}): expected a move, got %A{other}"

    let private isNoOp (platform : SimulatedUnixPlatform) (rows : (string * string) list) : unit =
        for source, destination in rows do
            match verdict platform CallerPrivilege.Unprivileged source destination with
            | RenameVerdict.NoOp -> ()
            | other -> failwith $"rename(%s{source}, %s{destination}): expected a no-op, got %A{other}"

    // ------------------------------------------------ what both kernels agree

    [<Test>]
    let ``the only thing that matters about either object is whether it is a directory`` () : unit =
        // The measured type matrix, collapsed: a symlink and a dangling symlink
        // behave as any other non-directory, because both walks are
        // `NoFollowFinal`.
        for platform in [ linux ; darwin ] do
            succeeds
                platform
                [
                    "f", "w/wf"
                    "f", "nx"
                    "f", "lf"
                    "f", "dang"
                    "lf", "w/wf"
                    "dang", "w/wf"
                    "d", "nx"
                    "d", "d2"
                    "dfull", "d2"
                ]

            refuses platform UnixError.EISDIR [ "f", "d" ; "f", "dfull" ; "lf", "d" ; "dang", "dfull" ]
            refuses platform UnixError.ENOTDIR [ "d", "f" ; "d", "lf" ; "dfull", "dang" ; "d", "w/wf" ]
            refuses platform UnixError.ENOTEMPTY [ "d", "dfull" ; "d2", "dfull" ]

    [<Test>]
    let ``a free source name is ENOENT whatever the destination is`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOENT [ "nx", "f" ; "nx", "d" ; "nx", "nx2" ; "nx", "w/wf" ; "p/nx", "w/x" ]

    [<Test>]
    let ``both names on one inode change nothing at all`` () : unit =
        // Including a non-empty directory renamed onto itself, which is what
        // puts the no-op above the emptiness check on both.
        for platform in [ linux ; darwin ] do
            isNoOp platform [ "f", "g" ; "g", "f" ; "f", "f" ; "d", "d" ; "dfull", "dfull" ; "d", "./d" ]

    [<Test>]
    let ``a symlink to a file renamed onto that file is not a no-op`` () : unit =
        // Two inodes, so the link really replaces its own target and is left
        // pointing at itself. It looks like a special case and is not.
        for platform in [ linux ; darwin ] do
            moves (verdict platform CallerPrivilege.Unprivileged "lf" "f")
            |> shouldEqual ("lf", "f")

    [<Test>]
    let ``a destination inside the source's own subtree is EINVAL on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EINVAL [ "a", "a/b" ; "a", "a/b/c" ; "a", "a/b/nx" ]

    [<Test>]
    let ``a name with the source as a string prefix is not an ancestor`` () : unit =
        // `isWithinSubtree` is on inodes rather than on path text, and this is
        // the row that would fail if it were not: "/a" is a string prefix of
        // "/ab".
        for platform in [ linux ; darwin ] do
            succeeds platform [ "a", "ab" ; "a", "ab2" ; "a/b/c", "a/x" ]

    [<Test>]
    let ``the subtree rule beats every permission check on both`` () : unit =
        // `p` is unwritable and holds `pm`, so a verdict that asked about the
        // source's parent first would answer EACCES.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EINVAL [ "p/pm", "p/pm/x" ]

    [<Test>]
    let ``an unsearchable directory is refused by the walk on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "nosearch/kid", "w/x" ; "f", "nosearch/x" ; "nosearch/nx", "w/x" ]

    [<Test>]
    let ``a free source name under an unwritable parent is ENOENT, not EACCES`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOENT [ "p/nx", "w/x" ]

    [<Test>]
    let ``moving a directory to a new parent needs write on the directory itself`` () : unit =
        // The ".." rewrite. `mv` is 0o555, so the move across parents is refused
        // and the rename within one parent is not — measured identically on both.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EACCES [ "mv", "w/x" ; "mv", "w/wd" ; "mv", "w/wfull" ]
            succeeds platform [ "mv", "mv2" ]

    [<Test>]
    let ``the moved directory's write bit is demanded on more occasions on Darwin`` () : unit =
        // Linux asks for it only when the parent changes -- that is the ".."
        // rewrite and nothing else. Darwin asks then, and also whenever the
        // moved directory *displaces* another directory, within one parent
        // included. Measured 40/40 per cell on both kernels.
        //
        // `mv` is 0o555 and sits at the root, alongside a free name, an empty
        // directory and a non-empty one.
        for platform in [ linux ; darwin ] do
            succeeds platform [ "mv", "mvfree" ]

        succeeds linux [ "mv", "mvdest" ]
        refuses darwin UnixError.EACCES [ "mv", "mvdest" ]

        // And it beats ENOTEMPTY on the same shape, which is what makes it a
        // check of its own rather than a spelling of the displaced-directory one.
        refuses linux UnixError.ENOTEMPTY [ "mv", "mvfull" ]
        refuses darwin UnixError.EACCES [ "mv", "mvfull" ]

    [<Test>]
    let ``the type rule beats the moved directory's own write bit on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTDIR [ "mv", "w/wf" ]

    [<Test>]
    let ``a destination whose parent has lost its last name is ENOENT on both`` () : unit =
        for platform in [ linux ; darwin ] do
            verdictIn orphanTree orphan platform CallerPrivilege.Unprivileged "/f" "x"
            |> shouldEqual (RenameVerdict.Refuse UnixError.ENOENT)

            // Above the source's parent's write bit on both, which is the one
            // ordering the two flavours agree about here.
            verdictIn orphanTree orphan platform CallerPrivilege.Unprivileged "/p/pf" "x"
            |> shouldEqual (RenameVerdict.Refuse UnixError.ENOENT)

    // ------------------------------------------- where the two flavours part

    [<Test>]
    let ``a path that consumed no final name is EBUSY on Linux and typed on Darwin`` () : unit =
        refuses linux UnixError.EBUSY [ "/", "f2" ; "d/.", "x" ; "d/..", "x" ; "a/b/.", "x" ; "a/b/..", "x" ]
        refuses darwin UnixError.EISDIR [ "/", "f2" ]
        // "d/.." and "/.." reach the *root* by a final "..", and that is
        // measured -- on an APFS disk image, where both operands share a device,
        // it is EINVAL like any other directory. Only a final "." at the root is
        // unmeasurable; see the refusal test below.
        refuses darwin UnixError.EINVAL [ "d/.", "x" ; "a/b/.", "x" ; "a/b/..", "x" ; "d/..", "x" ; "/..", "x" ]

    [<Test>]
    let ``a destination that consumed no final name is EBUSY on Linux and EINVAL on Darwin`` () : unit =
        refuses linux UnixError.EBUSY [ "f", "d/." ; "f", "d/.." ; "f", "/" ; "d", "/" ; "d", "a/b/.." ]
        refuses darwin UnixError.EINVAL [ "f", "d/." ; "f", "d/.." ; "d", "/" ; "d", "a/b/.." ]
        // Darwin does not specialise "/" as a destination at all: it falls to
        // the ordinary type rule, so the answer depends on the *source*'s kind.
        refuses darwin UnixError.EISDIR [ "f", "/" ]

    [<Test>]
    let ``a free source name beats the destination's navigation arm only on Darwin`` () : unit =
        refuses linux UnixError.EBUSY [ "nx", "d/." ]
        refuses darwin UnixError.ENOENT [ "nx", "d/." ]

    [<Test>]
    let ``the root is not a special case for either flavour`` () : unit =
        // Darwin's `unlink` and `rmdir` each give the root its own EBUSY arm, so
        // the absence of one here is worth asserting rather than assuming.
        //
        // Establishing it took an APFS disk image and some care, because the
        // obvious measurement is masked: a filesystem root that is not "/" is a
        // *mount* root, and renaming one is liable to EXDEV. Measured 40 trials
        // per row, all stable, the discriminator is not "." against ".." but
        // whether the source's parent and the destination's parent are the same
        // object -- `rename("base/.", "p/x")` and `rename("p/..", "base/x")`
        // both reach the mount root and both answer EINVAL, while the same
        // sources with the destination in the other directory answer EXDEV. So
        // where the mount boundary stays quiet, the root answers what any
        // directory answers, and PawPrint has no mounts to make it speak.
        refuses linux UnixError.EBUSY [ "/.", "x" ; "/..", "x" ; "lroot/.", "x" ; "d/..", "x" ]
        refuses darwin UnixError.EINVAL [ "/.", "x" ; "/..", "x" ; "lroot/.", "x" ; "d/..", "x" ]

        // And a non-root directory reached the same ways answers the same thing,
        // which is the whole content of "not a special case".
        refuses darwin UnixError.EINVAL [ "a/b/.", "x" ; "a/b/..", "x" ]
        refuses linux UnixError.EBUSY [ "a/b/.", "x" ; "a/b/..", "x" ]

    [<Test>]
    let ``the permission checks beat the type rule only on Linux`` () : unit =
        // `p` and `q` are unwritable; `w/wd` and `q/qd` are directories. Linux
        // asks about the parents first, Darwin about the types.
        refuses linux UnixError.EACCES [ "p/pf", "w/wd" ; "f", "q/qd" ]
        refuses darwin UnixError.EISDIR [ "p/pf", "w/wd" ; "f", "q/qd" ]

    [<Test>]
    let ``the destination parent's write bit beats emptiness only on Linux`` () : unit =
        refuses linux UnixError.EACCES [ "d", "q/qfull" ]
        refuses darwin UnixError.ENOTEMPTY [ "d", "q/qfull" ]

    [<Test>]
    let ``the no-op beats the permission checks only on Linux`` () : unit =
        isNoOp linux [ "p/pf", "p/ph" ; "p/ph", "p/pf" ; "p/pf", "p/pf" ; "p/pd", "p/pd" ]
        refuses darwin UnixError.EACCES [ "p/pf", "p/ph" ; "p/ph", "p/pf" ; "p/pf", "p/pf" ; "p/pd", "p/pd" ]

    [<Test>]
    let ``a directory displacing a directory consults different objects`` () : unit =
        // The strangest measured fact in this syscall, and the one no reordering
        // of a shared check could express. Darwin asks the *displaced*
        // directory for its write bit and never looks at the directory holding
        // it; Linux asks the holder and never looks at the displaced object.
        //
        //   w is writable, w/wzero is a mode-0000 empty directory
        //   q is unwritable, q/qd is an ordinary empty directory
        succeeds linux [ "d", "w/wzero" ]
        refuses darwin UnixError.EACCES [ "d", "w/wzero" ]

        refuses linux UnixError.EACCES [ "d", "q/qd" ]
        succeeds darwin [ "d", "q/qd" ]

    [<Test>]
    let ``the displaced directory's write bit beats emptiness on Darwin`` () : unit =
        // `w/wzerofull` is both unwritable and non-empty. Linux never asks about
        // its mode at all and reports the emptiness.
        refuses linux UnixError.ENOTEMPTY [ "d", "w/wzerofull" ]
        refuses darwin UnixError.EACCES [ "d", "w/wzerofull" ]

    [<Test>]
    let ``the self-rename of an unwritable directory diverges`` () : unit =
        // Darwin's displaced-directory check sees the directory as displacing
        // *itself*, and fires before the no-op below it.
        isNoOp linux [ "w/wzero", "w/wzero" ; "w/wzerofull", "w/wzerofull" ]
        refuses darwin UnixError.EACCES [ "w/wzero", "w/wzero" ; "w/wzerofull", "w/wzerofull" ]

    [<Test>]
    let ``a non-directory over a mode-zero directory is EISDIR on both`` () : unit =
        // The control for the two rows above: the divergence is about *which*
        // write bit is consulted, not about mode 0 being special, and the type
        // rule pre-empts it for a non-directory source on both flavours.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EISDIR [ "f", "w/wzero" ]

    [<Test>]
    let ``the orphaned destination parent beats the source's navigation only on Darwin`` () : unit =
        verdictIn orphanTree orphan linux CallerPrivilege.Unprivileged "/d/." "x"
        |> shouldEqual (RenameVerdict.Refuse UnixError.EBUSY)

        verdictIn orphanTree orphan darwin CallerPrivilege.Unprivileged "/d/." "x"
        |> shouldEqual (RenameVerdict.Refuse UnixError.ENOENT)

    // ---------------------------------------------------- trailing separators

    [<Test>]
    let ``a trailing separator on the source demands a directory`` () : unit =
        // Linux enforces the demand in the verdict, because its walk is
        // `Ignore`; Darwin's walk is `Demand` and has already answered. The two
        // agree on a plain file and part company on everything a link names.
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.ENOTDIR [ "f/", "x" ]
            refuses platform UnixError.ENOENT [ "nx/", "x" ]
            succeeds platform [ "d/", "x" ]

        refuses linux UnixError.ENOTDIR [ "lf/", "x" ; "dang/", "x" ; "lroot/", "x" ]
        refuses darwin UnixError.ENOTDIR [ "lf/", "x" ]
        refuses darwin UnixError.ENOENT [ "dang/", "x" ]
        refuses darwin UnixError.EISDIR [ "lroot/", "x" ]

    [<Test>]
    let ``a trailing separator on the source moves different objects`` () : unit =
        // The destructive divergence `Resolution.FinalSymlinkFollowed` warns
        // about, and the reason this syscall dispatches on the flavour rather
        // than picking a column: with `ld -> d`, Darwin moves `d` and Linux
        // refuses outright.
        refuses linux UnixError.ENOTDIR [ "ld/", "moved" ]

        moves (verdict darwin CallerPrivilege.Unprivileged "ld/" "moved")
        |> shouldEqual ("d", "moved")

    [<Test>]
    let ``a trailing separator on the destination demands a directory source`` () : unit =
        // Both kernels demand it; they disagree only on the errno for a *free*
        // destination name, because XNU passes WILLBEDIR to the destination
        // lookup exactly when the source is a directory.
        refuses linux UnixError.ENOTDIR [ "f", "nx/" ; "f", "d/" ]
        refuses darwin UnixError.ENOENT [ "f", "nx/" ]
        refuses darwin UnixError.EISDIR [ "f", "d/" ]

        for platform in [ linux ; darwin ] do
            succeeds platform [ "d", "nx/" ; "d2", "nx2/" ]

    [<Test>]
    let ``a trailing separator on the destination replaces different objects`` () : unit =
        // With `ld -> d`, Linux sees a symlink at the destination and refuses;
        // Darwin follows it and replaces `d` itself.
        refuses linux UnixError.ENOTDIR [ "d2", "ld/" ]

        moves (verdict darwin CallerPrivilege.Unprivileged "d2" "ld/")
        |> shouldEqual ("d2", "d")

    [<Test>]
    let ``the destination's trailing separator beats its parent's write bit`` () : unit =
        refuses linux UnixError.ENOTDIR [ "f", "q/nx/" ]
        refuses darwin UnixError.ENOENT [ "f", "q/nx/" ]

    [<Test>]
    let ``the destination's trailing separator demands nothing of the destination`` () : unit =
        // The arm above the write checks asks only about the *source*. Seeing
        // that needs an unwritable parent: with `q` writable, `rename(d, "q/l/")`
        // is ENOTDIR either way — from this arm if it asked, and from the type
        // rule below if it did not. Measured EACCES, so it does not ask.
        refuses linux UnixError.EACCES [ "d", "q/lq/" ; "d", "q/qfile/" ]
        // And the *source* half really is above them, on the same shape.
        refuses linux UnixError.ENOTDIR [ "f", "q/lq/" ]

        // Darwin's walk follows `q/lq` and lands on `d2`, so what it sees is an
        // ordinary directory-over-directory rename — which is the one shape
        // where it never consults `q` at all.
        moves (verdict darwin CallerPrivilege.Unprivileged "d" "q/lq/")
        |> shouldEqual ("d", "d2")

        refuses darwin UnixError.EISDIR [ "f", "q/lq/" ]
        // `q/qfile` is a regular file, so Darwin's `Demand` walk refuses the
        // path before any verdict runs.
        refuses darwin UnixError.ENOTDIR [ "d", "q/qfile/" ]

    [<Test>]
    let ``a symlink at the destination is refused by the type rule, not the separator`` () : unit =
        // The control for the row above: with a writable parent both candidate
        // arms answer ENOTDIR, which is why that row needs the narrowed one.
        refuses linux UnixError.ENOTDIR [ "d", "ld/" ]

    [<Test>]
    let ``the subtree rule beats the destination's trailing separator on both`` () : unit =
        for platform in [ linux ; darwin ] do
            refuses platform UnixError.EINVAL [ "a", "a/b/nx/" ]

    // --------------------------------------------------------------- privilege

    [<Test>]
    let ``privilege stops the permission arms firing and reorders nothing`` () : unit =
        // Measured at uid 0 on both: every EACCES row falls through to whatever
        // check was below it, and no other row moves.
        //
        // The second assertion is deliberately scoped to the rows that diverge
        // *because of a permission check*. The flavours do not agree on every
        // row at uid 0 — the navigation arms diverge there exactly as they do
        // at uid 1000, since privilege never touches a structural check.
        for platform in [ linux ; darwin ] do
            for source, destination in
                [
                    "p/pf", "w/x"
                    "f", "q/x"
                    "mv", "w/x"
                    "d", "w/wzero"
                    // Darwin's second moved-directory occasion: EACCES at uid
                    // 501, and measured `ok` at uid 0, so privilege disables
                    // this arm exactly as it disables the others.
                    "mv", "mvdest"
                ] do
                match
                    verdictIn tree (VirtualFileSystem.root tree) platform CallerPrivilege.Privileged source destination
                with
                | RenameVerdict.Move _ -> ()
                | other -> failwith $"rename(%s{source}, %s{destination}) at uid 0: expected a move, got %A{other}"

        for source, destination in
            [
                "p/pf", "w/wd"
                "f", "q/qd"
                "d", "q/qfull"
                "p/pf", "p/ph"
                "d", "w/wzero"
                "d", "q/qd"
                "w/wzero", "w/wzero"
                "d", "w/wzerofull"
                // At uid 501 this is EACCES on Darwin and ENOTEMPTY on Linux;
                // measured at uid 0, both answer ENOTEMPTY -- the moved-directory
                // arm stops firing and the emptiness check below it speaks.
                "mv", "mvfull"
            ] do
            let onLinux =
                verdictIn tree (VirtualFileSystem.root tree) linux CallerPrivilege.Privileged source destination

            let onDarwin =
                verdictIn tree (VirtualFileSystem.root tree) darwin CallerPrivilege.Privileged source destination

            if onLinux <> onDarwin then
                failwith
                    $"rename(%s{source}, %s{destination}) at uid 0: Linux says %A{onLinux} and Darwin says %A{onDarwin}, but every measured row agrees at uid 0."

    [<Test>]
    let ``an unsearchable directory still refuses a privileged caller nothing`` () : unit =
        for platform in [ linux ; darwin ] do
            match
                verdictIn tree (VirtualFileSystem.root tree) platform CallerPrivilege.Privileged "nosearch/kid" "w/x"
            with
            | RenameVerdict.Move _ -> ()
            | other -> failwith $"expected a move at uid 0, got %A{other}"

    // ----------------------------------------------------------- the platform

    [<Test>]
    let ``each flavour resolves both paths under its own walk`` () : unit =
        (SimulatedUnixPlatform.renameRules linux).TrailingSeparator
        |> shouldEqual TrailingSeparatorPolicy.Ignore

        (SimulatedUnixPlatform.renameRules darwin).TrailingSeparator
        |> shouldEqual TrailingSeparatorPolicy.Demand
