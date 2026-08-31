namespace WoofWare.PosixKernel

/// The order in which `rename(2)` resolves its two paths, which is
/// guest-visible because the two kernels answer different errnos when both
/// paths are bad.
///
/// Measured with pairs that *disagree* — a pair answering one errno either way
/// proves nothing. `rename(absent, "regularfile/x")` is ENOTDIR on Linux and
/// ENOENT on Darwin; `rename("<300 bytes>", "nodir/x")` is ENOENT on Linux and
/// ENAMETOOLONG on Darwin.
///
/// Not derivable from `RenameRules.TrailingSeparator`, and not something the
/// verdict could express: it decides which resolutions are *performed at all*,
/// before there are two `Resolution`s to judge.
[<RequireQualifiedAccess>]
type RenameWalkOrder =
    /// Both parents before either final lookup, with each pathname copied in
    /// immediately before its own parent is walked: the shape of Linux's
    /// `do_renameat2`, whose `filename_parentat(dfd, getname(name), ...)`
    /// propagates `getname`'s error out of the parent walk it was handed to.
    /// Every refusal either path earns after that is the verdict's, judged
    /// against both.
    ///
    /// The source's parent is resolved before the destination's pathname is
    /// even copied in: `rename("nodir/kid", <over PATH_MAX>)` is ENOENT while
    /// `rename("nope", <over PATH_MAX>)` is ENAMETOOLONG. A free source name
    /// cannot see that, being no parent-walk failure at all — only a source
    /// whose parent walk fails discriminates.
    ///
    /// Everything about the source's *final component* loses to the
    /// destination's parent — measured across a free name, "/", ".", "..", a
    /// trailing separator and a 300-byte name, all of which answer the
    /// destination's ENOTDIR.
    | ParentsThenFinals
    /// The source finished before the destination is looked at at all — its
    /// pathname included, and including the two refusals Darwin's source-side
    /// `namei` makes for itself under rename semantics. See
    /// `RenameRules.sourceScreen`.
    | SourceThenDestination

/// Everything a kernel does differently when `rename(2)` moves a name.
///
/// Two fields, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RenameRules.linuxVerdict` and
/// `RenameRules.darwinVerdict`, for the reason `UnlinkRules.verdict` gives.
/// `rename` diverges more than any operation before it: the two flavours
/// disagree about where the permission checks sit, about *which* directory's
/// write bit a directory-over-directory rename even consults, and about where
/// the no-op sits.
///
/// Measured on macOS 26.6/APFS at uid 501 and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row; `docs/probes/rename/` holds the probes.
type RenameRules =
    {
        /// The walk `rename` resolves *both* of its paths with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s and `rmdir`'s are.
        ///
        /// One field for two paths because, measured, each kernel resolves its
        /// source and its destination under the same policy — there is no row
        /// where a separator costs one path something it does not cost the
        /// other.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**, the divergence `Resolution.FinalSymlinkFollowed` warns
        /// about. With `s -> real` a directory, `rename("s/", "moved")` moves
        /// *real* on Darwin, leaving `s` dangling, and is ENOTDIR on Linux;
        /// `rename("src", "s/")` replaces *real* on Darwin and is ENOTDIR on
        /// Linux.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Which path is resolved first, and how far, before the other is
        /// looked at. See `RenameWalkOrder`.
        WalkOrder : RenameWalkOrder
    }

/// What `rename(2)` should do next, once both of its paths have been resolved.
[<RequireQualifiedAccess>]
type RenameVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Both paths name one inode. Succeed, and change nothing at all — not a
    /// binding, not a timestamp.
    ///
    /// A case here rather than a short-circuit in `VirtualFileSystem.rename`
    /// because its *position* is one of the things the flavours disagree about:
    /// Linux answers success for a no-op whose parent the caller may not write,
    /// and Darwin answers EACCES for the same call.
    | NoOp
    /// Move `sourceName` out of `sourceDirectory` and bind it as
    /// `destinationName` in `destinationDirectory`, displacing whatever is bound
    /// there.
    ///
    /// Carries no inode, though the verdict read several to decide: the moving
    /// code gets the displaced one from `VirtualFileSystem.rename`, which
    /// answers what it actually displaced, so there is one source for "which
    /// inode lost a name" and it is the one the move performed.
    | Move of
        sourceDirectory : InodeNumber *
        sourceName : DirectoryEntryName *
        destinationDirectory : InodeNumber *
        destinationName : DirectoryEntryName

/// The questions `rename(2)` asks about the four directories it can refuse for.
[<RequireQualifiedAccess>]
module private RenameChecks =
    /// Whether `inode` refuses this caller the write bit.
    ///
    /// Asked of four different directories — the source's parent, the
    /// destination's parent, the moved directory (whose ".." a change of parent
    /// rewrites) and, on Darwin only, the directory a directory displaces — so
    /// `role` names which, for the crash message. Only the owner triple can ever
    /// apply, since `stat` reports `Kernel.UserId` as every inode's `st_uid`,
    /// and the sticky bit can never refuse for the same reason.
    ///
    /// Partial in `inode`, which every caller has just obtained from a
    /// resolution or from a directory entry.
    let lacksWrite
        (role : string)
        (privilege : CallerPrivilege)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : bool
        =
        match VirtualFileSystem.tryGet inode vfs with
        | Some entry ->
            match Inode.permissions entry with
            | InodePermissions.Stored bits -> PermissionBits.deniedTo privilege AccessRequest.Write bits
            | InodePermissions.PlatformSymlinkDefault ->
                failwith
                    $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which reports platform-default symlink permissions -- but rename only asks this of a directory (this is an interpreter bug)."
        | None ->
            failwith
                $"RenameChecks.lacksWrite: %s{role} is inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// The inode the destination name is bound to, when that inode is a
    /// directory. `None` covers both "the name is free" and "it names something
    /// that is not a directory", which no arm needs to tell apart — the arms
    /// that care about a non-directory ask `namesNonDirectory` instead.
    let existingDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : InodeNumber option =
        existing |> Option.filter (fun inode -> RemovalChecks.isDirectory inode vfs)

    /// Whether the name is bound, and bound to something that is not a
    /// directory. False for a free name, which is what separates this from
    /// `not existingDirectory.IsSome`.
    let namesNonDirectory (existing : InodeNumber option) (vfs : VirtualFileSystem) : bool =
        match existing with
        | Some inode -> not (RemovalChecks.isDirectory inode vfs)
        | None -> false

[<RequireQualifiedAccess>]
module RenameRules =
    /// The refusals the source earns *before* the destination's final name is
    /// looked up — `None` when it earns none yet and the verdict will judge it
    /// against both paths.
    ///
    /// Both flavours have one; they differ in content and in *where* it runs,
    /// which is what `RenameWalkOrder` decides. Darwin's is part of resolving
    /// the source, and runs before the destination's pathname has been read at
    /// all. Linux's runs after both parents and the source's own final lookup,
    /// and before the destination's — so it beats the destination's `NAME_MAX`
    /// and the orphaned-destination-parent arm, and loses to everything either
    /// parent walk can refuse.
    ///
    /// The arms, and the rows that pin each (see `docs/probes/rename/`):
    ///
    ///  * A source whose final name is free is ENOENT on both. Linux:
    ///    `rename("nope", <300-byte name>)` is ENOENT, not the destination's
    ///    ENAMETOOLONG. Darwin: `rename("nope", "f/x")` is ENOENT, not the
    ///    destination's ENOTDIR.
    ///  * A source that consumed no final name is EBUSY on Linux, whichever
    ///    navigation reached it — measured against an orphaned destination
    ///    parent, where `rename("d/.", "x")` is EBUSY and every other source
    ///    there is ENOENT. Darwin spends EISDIR on the bare root alone: "/." ,
    ///    "/.." and "/dev/.." all wait for the verdict, so it is the navigation
    ///    that is early rather than the inode.
    ///
    /// These arms are the verdicts' too, which is not duplication to remove: a
    /// verdict handed two resolutions must still answer them, and this says when
    /// its flavour gets to ask. Reached with the question already settled, the
    /// verdict's copies simply never fire.
    let sourceScreen (order : RenameWalkOrder) (source : Resolution) : UnixError option =
        match source.Target with
        | ResolvedTarget.Entry (_, _, Some _) -> None
        | ResolvedTarget.Entry (_, _, None) -> Some UnixError.ENOENT
        | ResolvedTarget.Directory (_, reachedBy) ->

        match order with
        // Linux spends one errno on all six navigation positions, source and
        // destination alike; see `linuxVerdict`.
        | RenameWalkOrder.ParentsThenFinals -> Some UnixError.EBUSY
        | RenameWalkOrder.SourceThenDestination ->

        match reachedBy with
        | FinalNavigation.Root -> Some UnixError.EISDIR
        | FinalNavigation.Current
        | FinalNavigation.Parent -> None

    /// Linux's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path — either path — that consumed no final name, so "/", "." and
    ///    ".." and any symlink expansion of them, is EBUSY. Linux spends one
    ///    errno on all six positions where `rmdir` spends three: `rename("/", x)`,
    ///    `rename("/.", x)`, `rename("d/.", x)`, `rename("d/..", x)`,
    ///    `rename(x, "d/.")` and `rename(dir, "/")` are all EBUSY. The source is
    ///    asked before the destination, which no row can see, since they share
    ///    an errno.
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, exactly as `mkdir` and `open(O_CREAT)` answer — and it beats
    ///    the source's trailing-separator demand and the write checks below.
    ///    Only reachable when the current directory is that orphan.
    ///  * A free source name is ENOENT, and beats the trailing-separator arms:
    ///    `rename("nope/", "g")` is ENOENT where `rename("f/", "g")` is ENOTDIR.
    ///  * A trailing separator on the **source** demands the source be a
    ///    directory: ENOTDIR otherwise. This is the arm Linux's walk declines to
    ///    make (`TrailingSeparatorPolicy.Ignore`), so it never traverses a final
    ///    symlink to get here — `rename("lf/", "g")`, `rename("dang/", "g")` and
    ///    `rename("lroot/", "g")` are all ENOTDIR, with no chance of moving a
    ///    link's target.
    ///  * A trailing separator on the **destination** demands that the *source*
    ///    be a directory: `rename(f, "absent/")` and `rename(f, "d/")` are
    ///    ENOTDIR, and so is `rename(p/f, "q/absent/")` with `q` unwritable,
    ///    which is what puts this arm above the write checks.
    ///
    ///    It demands nothing of the destination, and seeing that needs an
    ///    unwritable parent: `rename(d, "q/l/")` with `l` a symlink to a
    ///    directory is ENOTDIR when `q` is writable and **EACCES** when it is
    ///    not. So the ENOTDIR there is the ordinary type rule further down, not
    ///    this arm — the two are indistinguishable until a check between them
    ///    fires.
    ///  * Both paths naming one inode changes nothing and succeeds, and that
    ///    beats every permission check below: `rename(f, g)` with `g` a hard link
    ///    to `f` succeeds from a parent the caller may not write, and so does the
    ///    self-rename of a non-empty directory.
    ///  * A destination directory inside the source's own subtree is EINVAL, and
    ///    it beats *both* write checks — `rename(p/a, p/a/b)` is EINVAL with `p`
    ///    unwritable — as well as the type arm below: `rename(a, a/b/f)` with
    ///    `a/b/f` a regular file is EINVAL, not ENOTDIR.
    ///  * Each parent must grant write: EACCES. Above the type arm, which is
    ///    where Linux and Darwin part company — `rename(p/f, q/dir)` with `p`
    ///    unwritable is EACCES here and EISDIR on Darwin.
    ///  * Then the type rule: a directory over a non-directory is ENOTDIR, a
    ///    non-directory over a directory is EISDIR. A symlink is a
    ///    non-directory whatever it points at, since both walks are
    ///    `NoFollowFinal`.
    ///  * Moving a **directory to a different parent** rewrites its own ".."
    ///    entry, so it demands write on the moved directory itself: EACCES.
    ///    Renaming one within its parent changes nothing inside it and demands
    ///    nothing -- and that holds even when it *displaces* a directory there,
    ///    which is measured (40/40) and is where Darwin diverges a second time. This check is *below* the type arm, unlike the parents' —
    ///    `rename(p/m, q/file)` with `p/m` unwritable is ENOTDIR — and above
    ///    ENOTEMPTY.
    ///  * A destination directory that still holds an entry is ENOTEMPTY.
    ///
    /// Linux never consults the mode of the thing being displaced: measured,
    /// `rename(dir, emptydir)` succeeds with the destination at mode 0. That is
    /// the arm Darwin has and this one does not.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bits
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match source.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match destination.Target with
        | ResolvedTarget.Directory _ -> RenameVerdict.Refuse UnixError.EBUSY
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        if VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs
        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if source.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destination.TrailingSeparatorDemanded && not movedIsDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            movedIsDirectory
            && sourceDirectory <> destinationDirectory
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Darwin's `rename(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A destination whose parent directory has lost its own last name is
    ///    ENOENT, and on this flavour that beats *everything*, including the
    ///    source's navigation refusal: from inside an `rmdir`'d current
    ///    directory, `rename("d/.", "x")` is ENOENT where it is EINVAL from
    ///    anywhere else.
    ///  * A **source** that consumed no final name: "/" is EISDIR, and any
    ///    directory reached by "." or ".." is EINVAL -- the root included, which
    ///    took an APFS disk image to establish because EXDEV masks it on some
    ///    approaches. See the arm for the rows. Where Linux spends EBUSY on all
    ///    of them.
    ///  * A free source name is ENOENT, and beats the destination's navigation
    ///    arm below: `rename("nope", "d/.")` is ENOENT here and EBUSY on Linux.
    ///  * A **destination** that consumed no final name: "." and ".." are EINVAL
    ///    whatever the source is and whatever they reached — measured with a ".."
    ///    that is not an ancestor of the source, so the rule is about the
    ///    component rather than about ancestry. "/" is not special-cased and
    ///    falls to the type rule: `rename(file, "/")` is EISDIR while
    ///    `rename(dir, "/")` is EINVAL.
    ///  * The type rule, which on this flavour is above everything below it: a
    ///    directory over a non-directory is ENOTDIR, a non-directory over a
    ///    directory is EISDIR. `rename(p/f, q/dir)` with `p` unwritable is EISDIR
    ///    here and EACCES on Linux, and `rename(a, a/b/f)` with `a/b/f` a file is
    ///    ENOTDIR here and EINVAL on Linux.
    ///  * A trailing separator on the destination, over a name that is *free*,
    ///    demands that the source be a directory: ENOENT otherwise.
    ///    `rename(f, "absent/")` is ENOENT where `rename(d, "absent/")` succeeds
    ///    — XNU passes `WILLBEDIR` to the destination lookup exactly when the
    ///    source is a directory. Linux answers the same shape ENOTDIR. The
    ///    source's own separator needs no arm: Darwin's walk is `Demand` and has
    ///    already refused it.
    ///  * A destination directory inside the source's own subtree is EINVAL,
    ///    beating both write checks below.
    ///  * The source's parent must grant write: EACCES. Above the no-op, which
    ///    is the arm Linux orders the other way round — `rename(f, g)` with `g` a
    ///    hard link to `f` is EACCES here from an unwritable parent, and succeeds
    ///    on Linux.
    ///  * Then a write check on the destination side, and *which* directory it
    ///    asks about is the strangest measured fact in this syscall: when a
    ///    directory replaces an existing directory, Darwin consults the write bit
    ///    of the **directory being displaced** and never looks at its parent at
    ///    all. Measured four ways — with the parent at 0o555 and the displaced
    ///    directory at 0o755 it succeeds, at 0o755 and 0o000 it is EACCES, at
    ///    0o555 and 0o300 it succeeds, and a control confirms the parent really
    ///    does refuse an ordinary create. Every other shape consults the
    ///    destination's parent as Linux does.
    ///  * Both paths naming one inode changes nothing and succeeds — below the
    ///    two write checks above, which is why the self-rename of a directory
    ///    whose own write bit is missing is EACCES here and succeeds on Linux.
    ///  * Moving a directory demands write on the moved directory -- on *two*
    ///    occasions where Linux wants one. Linux asks only when the parent
    ///    changes, which is the ".." rewrite; Darwin asks then and also whenever
    ///    the moved directory displaces another directory, within one parent
    ///    included. Measured 40/40: `rename("p/m", "p/d")` with `m` at 0o555 and
    ///    `d` an existing directory is EACCES here and succeeds on Linux, while
    ///    the same call to a free name succeeds on both.
    ///  * A destination directory that still holds an entry is ENOTEMPTY, below
    ///    the displaced-directory write check: `rename(dir, fulldir)` with the
    ///    non-empty destination at mode 0 is EACCES here and ENOTEMPTY on Linux.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over an
    /// *existing* non-directory never reaches here — the walk has already
    /// answered ENOTDIR, ELOOP or ENOENT. What does reach here is a separator
    /// over a directory a final symlink named, and that is the destructive row:
    /// `rename("s/", "moved")` moves the link's target.
    let private darwinVerdict
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        let destinationParentIsOrphan =
            match destination.Target with
            | ResolvedTarget.Entry (destinationDirectory, _, _) ->
                VirtualFileSystem.isOrphanedDirectory destinationDirectory vfs
            | ResolvedTarget.Directory _ -> false

        if destinationParentIsOrphan then
            RenameVerdict.Refuse UnixError.ENOENT
        else

        match source.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RenameVerdict.Refuse UnixError.EISDIR
            // No root special case, unlike Darwin's `unlink` and `rmdir`, which
            // each give the root its own EBUSY arm. Establishing that took some
            // care, because the obvious measurement is masked: a filesystem root
            // that is not "/" is a *mount* root, and renaming one is liable to
            // EXDEV.
            //
            // Measured on a fresh APFS image, 40 trials per row, all stable. The
            // discriminator turns out not to be "." against ".." but whether the
            // source's parent directory and the destination's parent directory
            // are the same object: with `p` a directory inside the mount,
            // `rename("base/.", "p/x")` and `rename("p/..", "base/x")` both reach
            // the mount root and both answer **EINVAL**, while the same two
            // sources with the destination in the other directory answer EXDEV.
            // So EXDEV is the mount boundary talking, and where it stays quiet
            // the root answers exactly what any other directory answers.
            //
            // PawPrint has one filesystem and no mounts, so nothing here can
            // produce EXDEV and the EINVAL readings are the applicable ones.
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
        | ResolvedTarget.Entry (sourceDirectory, sourceName, sourceExisting) ->

        match sourceExisting with
        | None -> RenameVerdict.Refuse UnixError.ENOENT
        | Some moved ->

        let movedIsDirectory = RemovalChecks.isDirectory moved vfs

        match destination.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Current
            | FinalNavigation.Parent -> RenameVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Root ->
                if movedIsDirectory then
                    RenameVerdict.Refuse UnixError.EINVAL
                else
                    RenameVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (destinationDirectory, destinationName, destinationExisting) ->

        let displacedDirectory = RenameChecks.existingDirectory destinationExisting vfs
        let displacesNonDirectory = RenameChecks.namesNonDirectory destinationExisting vfs

        if movedIsDirectory && displacesNonDirectory then
            RenameVerdict.Refuse UnixError.ENOTDIR
        elif not movedIsDirectory && displacedDirectory.IsSome then
            RenameVerdict.Refuse UnixError.EISDIR
        elif
            destination.TrailingSeparatorDemanded
            && destinationExisting.IsNone
            && not movedIsDirectory
        then
            RenameVerdict.Refuse UnixError.ENOENT
        elif
            movedIsDirectory
            && VirtualFileSystem.isWithinSubtree moved destinationDirectory vfs
        then
            RenameVerdict.Refuse UnixError.EINVAL
        elif RenameChecks.lacksWrite "the source's parent" privilege sourceDirectory vfs then
            RenameVerdict.Refuse UnixError.EACCES
        elif
            // Which directory this asks about is the measured oddity. A
            // directory displacing a directory is the one shape where Darwin
            // consults the displaced object rather than the directory holding
            // it -- and `displacedDirectory` being `Some` here already implies
            // the source is a directory, because the EISDIR arm above refused
            // the only other way to reach this line with one.
            (match displacedDirectory with
             | Some displaced -> RenameChecks.lacksWrite "the displaced directory" privilege displaced vfs
             | None -> RenameChecks.lacksWrite "the destination's parent" privilege destinationDirectory vfs)
        then
            RenameVerdict.Refuse UnixError.EACCES
        elif destinationExisting = Some moved then
            RenameVerdict.NoOp
        elif
            // Two occasions, not one, and this is where Darwin parts from Linux
            // a second time. Linux wants this bit only when the parent changes,
            // which is the ".." rewrite and nothing else. Darwin wants it then
            // *and* whenever the moved directory displaces another directory,
            // even within one parent: measured 40/40, `rename("p/m", "p/d")`
            // with `m` at 0o555 and `d` an existing directory is EACCES, where
            // the same call to a free name succeeds and Linux allows both.
            //
            // It beats ENOTEMPTY below on the same shape -- a non-empty `d` is
            // still EACCES -- which is what makes it a check in its own right
            // rather than a spelling of the displaced-directory one above.
            movedIsDirectory
            && (sourceDirectory <> destinationDirectory || displacedDirectory.IsSome)
            && RenameChecks.lacksWrite "the moved directory" privilege moved vfs
        then
            RenameVerdict.Refuse UnixError.EACCES
        else

        match displacedDirectory with
        | Some displaced when not (RemovalChecks.isEmptyDirectory displaced vfs) ->
            RenameVerdict.Refuse UnixError.ENOTEMPTY
        | Some _
        | None -> RenameVerdict.Move (sourceDirectory, sourceName, destinationDirectory, destinationName)

    /// Decide what a `rename(2)` owes, given how its two paths resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rename` makes the case more strongly than either removal did, because
    /// here the flavours do not even agree on *which object* a check is about —
    /// a directory displacing a directory consults the displaced directory's
    /// write bit on Darwin and its parent's on Linux, which is not a reordering
    /// of one check but two different checks.
    ///
    /// `source` and `destination` must both have been resolved under
    /// `SimulatedUnixPlatform.renameRules`' `TrailingSeparator` and
    /// `SymlinkPolicy.NoFollowFinal`, which is what makes the trailing-separator
    /// arms above mean what they say.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (source : Resolution)
        (destination : Resolution)
        (vfs : VirtualFileSystem)
        : RenameVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege source destination vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege source destination vfs
