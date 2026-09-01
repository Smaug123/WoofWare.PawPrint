namespace WoofWare.PosixKernel

/// <summary>
/// Whether this syscall follows symlinks found in the <i>final</i> position.
/// </summary>
/// <remarks>
/// Symlinks in every earlier position are always followed.
///
/// A trailing separator can override this (see <c>TrailingSeparatorPolicy</c>).
/// </remarks>
[<RequireQualifiedAccess>]
type SymlinkPolicy =
    /// <summary>
    /// Follow it.
    /// </summary>
    /// <example>
    /// This is what <c>stat</c>, or <c>open</c> without <c>O_NOFOLLOW</c>, do.
    /// </example>
    | Follow
    /// <summary>
    /// Stop at the link itself.
    /// </summary>
    /// <example>
    /// This is what <c>lstat</c>, <c>readlink</c>, <c>unlink</c>, <c>rename</c>,
    /// and <c>open</c> with <c>O_NOFOLLOW</c> do.
    /// </example>
    | NoFollowFinal

/// What a trailing separator on the *final* component means to this walk.
///
/// A second axis from `SymlinkPolicy`, and independent of it: this one is not
/// about what the walk follows, but about whether "the final component must name
/// a directory" is a demand to record or a refusal to make.
///
/// The two Unixes split here for a *creating* open, and the split is not a
/// choice PawPrint makes — it is measured. Linux refuses such a path outright,
/// XNU resolves it as any other lookup would.
[<RequireQualifiedAccess>]
type TrailingSeparatorPolicy =
    /// Record the demand on `Resolution.TrailingSeparatorDemanded` and let the
    /// caller enforce it. Every lookup (`stat`, `lstat`, `readlink`), every
    /// non-creating `open`, and -- on Darwin -- a creating `open` and `mkdir`.
    | Demand
    /// Answer EISDIR on *reaching* a final component that carries a trailing
    /// separator — before that component's length is checked, before it is
    /// looked up, and before any symlink it names is traversed. What Linux does
    /// for a creating open.
    ///
    /// Measured on Linux, and it is the *position* of the check that these rows
    /// pin rather than the errno: `<300 a>/` is EISDIR, not ENAMETOOLONG;
    /// `cyc/` is EISDIR, not ELOOP; `d/` under `O_EXCL` is EISDIR, not EEXIST;
    /// `dang/` is EISDIR, not ENOENT. It does *not* pre-empt failures on earlier
    /// components: `nodir/new/` is ENOENT and `f/new/` is ENOTDIR, because the
    /// walk never reaches the final component in either.
    ///
    /// The separator may equally have arrived from a spliced symlink target
    /// rather than from the guest's own path: `l -> "cyc2/"` opened with
    /// `O_CREAT` is EISDIR rather than ELOOP, so the check has to sit inside the
    /// walk rather than at the syscall boundary.
    | RefuseIsDirectory
    /// Record the demand on `Resolution.TrailingSeparatorDemanded` and impose
    /// *nothing*: the final component is neither dereferenced because of the
    /// separator nor required to be a directory. What Linux's `mkdir` does,
    /// whose last component is resolved by `filename_create` -- a plain dentry
    /// lookup that never follows a link and never inspects what it found.
    ///
    /// Measured, and it is two suppressions rather than one. Under Linux
    /// `mkdir`, "lf/", "ld/", "dang/" and "cyc/" are all EEXIST -- no traversal,
    /// so no ELOOP and no chance to create a dangling link's target -- *and*
    /// "f/" is EEXIST rather than the ENOTDIR every lookup owes it. Darwin's
    /// `mkdir` wants `Demand` instead, and answers ENOTDIR, ELOOP, and (for
    /// "dang/") creates the link's target.
    ///
    /// The two suppressions co-occur here because Linux's creating lookup does
    /// neither, and they are *not* the same fact. Linux's deletion wants
    /// no-follow *with* the directory demand — `unlink("ld/")` and `unlink("f/")`
    /// are both ENOTDIR there — which looks like a second axis, and is not one:
    /// `do_unlinkat` takes a parent and a name and then inspects the byte after
    /// the name, so the demand is enforced *after* the walk, by the verdict,
    /// out of `Resolution.TrailingSeparatorDemanded`. Measured, and the row
    /// that proves it is `unlink("lroot/")` with `lroot -> "/"`: ENOTDIR on
    /// Linux, so the link was never traversed, where Darwin's `Demand` walk
    /// traverses it and answers EISDIR.
    ///
    /// So this stays one-dimensional, and `unlink` selects it here on Linux
    /// while enforcing the demand itself. See `UnlinkRules.verdict`.
    | Ignore

/// Which component a resolution last consumed, for the paths that end without
/// a name to look up.
///
/// Carried on `ResolvedTarget.Directory` rather than left to the caller to read
/// off its own path, because a symlink expansion replaces the final component:
/// with `l1 -> "."` and `l2 -> "d/.."`, the paths "l1/" and "l2/" are the same
/// shape but land on different navigation. Probed on macOS, `rmdir` owes them
/// different errnos — EINVAL and ENOTEMPTY respectively — so the distinction is
/// guest-observable and unrecoverable from the original path.
[<RequireQualifiedAccess>]
type FinalNavigation =
    /// The path named no component at all: "/" itself, or a symlink whose
    /// target was "/". `rmdir` owes this EBUSY on Linux and EISDIR on Darwin.
    | Root
    /// The last component consumed was ".". `rmdir` owes this EINVAL — except
    /// on Darwin at the root itself, which is EBUSY.
    | Current
    /// The last component consumed was "..". `rmdir` owes this ENOTEMPTY —
    /// except on Darwin at the root itself, which is EBUSY.
    | Parent

/// Where a path resolution ended up.
[<RequireQualifiedAccess>]
type ResolvedTarget =
    /// The path named `Name` inside `Directory`. `Existing` is the inode bound
    /// to that name, or `None` when the name is free — which is *not* an error
    /// here, because it is exactly the state `open(O_CREAT)`, `mkdir` and
    /// `symlink` need, and callers that require the file to exist report
    /// ENOENT themselves.
    | Entry of directory : InodeNumber * name : DirectoryEntryName * existing : InodeNumber option
    /// The path resolved straight to a directory with no final name to look
    /// up, because its last component — after any symlink expansion — was "/",
    /// "." or "..".
    ///
    /// `ReachedBy` says which, because the errno that follows depends on it and
    /// the caller cannot recover it from the path it passed in: see
    /// `FinalNavigation`. (ENOTEMPTY is itself platform-dependent: Linux 39,
    /// Darwin 66.)
    | Directory of inode : InodeNumber * reachedBy : FinalNavigation

/// The outcome of a resolution, together with the facts about *how* it
/// finished that a caller cannot recover from the path it passed in.
type Resolution =
    {
        Target : ResolvedTarget
        /// The path, after any final symlink was spliced in, ended with a
        /// separator — so it demanded that its final component be a directory.
        ///
        /// Not simply the caller's `UnixPath.hasTrailingSeparator`: following a
        /// final symlink replaces the final path segment, so a link whose
        /// target is "d/" imposes the demand even when the guest's own path did
        /// not.
        ///
        /// What the demand *costs* is the walk's business under
        /// `TrailingSeparatorPolicy.Demand` (an existing non-directory final
        /// gives ENOTDIR) and under `RefuseIsDirectory` (EISDIR on reaching the
        /// component). Under `Ignore` it costs nothing and this flag is a report
        /// about the path rather than about anything enforced. The part that is
        /// left to the caller either way — see `FinalSymlinkFollowed`.
        TrailingSeparatorDemanded : bool
        /// A symlink in the *final* position was followed to get here.
        ///
        /// In combination with `TrailingSeparatorDemanded`, this pair is where
        /// Linux and macOS disagree destructively. Probed on macOS: with `ld -> realdir`,
        /// `rmdir("ld/")` *removes realdir*; with `dang -> nx`, `mkdir("dang/")`
        /// *creates nx*. Linux refuses both (ENOTDIR, EEXIST). A mutating
        /// operation that sees both flags set must therefore never pick a
        /// platform *unconditionally*, since the two choices destroy different
        /// objects: it either dispatches on the flavour, or fails loudly.
        ///
        /// Dispatching is only honest once both columns are measured at that
        /// operation's own scale, which is a per-syscall question rather than a
        /// property of this flag. `mkdir` is measured on both and dispatches
        /// (`SimulatedUnixPlatform.mkDirRules` picks the walk that reproduces
        /// each), and it never sees this flag set, because Linux's walk is
        /// `TrailingSeparatorPolicy.Ignore` and Darwin's acts on the following
        /// rather than reporting it. `unlink` is measured on both and dispatches
        /// the same way (`SimulatedUnixPlatform.unlinkRules`); it destroys
        /// nothing when the pair is set, since Darwin answers EPERM for the
        /// directory a followed link named and Linux never follows.
        ///
        /// `rmdir` is where the pair finally costs something. Measured on both
        /// and dispatched by `SimulatedUnixPlatform.rmDirRules`: with
        /// `ld -> d` and `d` empty, `rmdir("ld/")` removes `d` on Darwin and is
        /// ENOTDIR on Linux. Every mutating operation PawPrint models is now
        /// measured on both columns, so nothing here owes a loud failure any
        /// more — but a *new* one still does, and for the same reason.
        ///
        /// Lookup operations (`stat`, `lstat`) are unanimous and can ignore
        /// this.
        FinalSymlinkFollowed : bool
    }

/// A path resolution that has walked as far as the directory holding the final
/// name and stopped there, without looking that name up.
///
/// The point of pausing is that a syscall resolving *two* paths can interleave
/// them. Linux's `rename` walks both parents before it looks either final
/// component up, which is guest-visible: `rename("<300 bytes>", "nodir/x")` is
/// ENOENT because the destination's parent fails before the source's final name
/// is length-checked, while `rename("nope", "<300 bytes>")` is ENOENT the other
/// way round. One `resolveFull` cannot answer both, because it does the parent
/// walk and the final lookup in one indivisible call.
///
/// Opaque on purpose: the only thing to do with one is hand it to
/// `PathWalk.completeResolution`. There is no "the parent directory"
/// to read off it, because following a final symlink moves the directory the
/// name is looked up in, and that happens during the second half.
///
/// It carries the rules the walk began under and the filesystem it began
/// against, so a resumption cannot be given different ones.
[<NoEquality ; NoComparison>]
type PausedResolution =
    private
        {
            Limits : PathLimits
            Privilege : CallerPrivilege
            Policy : SymlinkPolicy
            TrailingSeparator : TrailingSeparatorPolicy
            FileSystem : VirtualFileSystem
            /// The directory the final name will be looked up in.
            Directory : InodeNumber
            /// The name still to look up, and the cursor positioned after it —
            /// which is what a splice of a final symlink is measured against.
            /// `None` when the path ran out of names before any could be looked
            /// up, which is the "/", "." and ".." case.
            Final : (DirectoryEntryName * PathCursor) option
            Trailing : bool
            FinalSymlinkFollowed : bool
            LastNavigation : FinalNavigation
            SymlinksTraversed : int
        }

[<RequireQualifiedAccess>]
module PathWalk =

    /// Expand a symbolic link into the walk's pathname buffer: the directory the
    /// expansion resumes from, and the buffer that replaces the one being
    /// walked.
    ///
    /// Refuses ELOOP when the traversal budget is spent and ENAMETOOLONG when
    /// the expansion would not fit, in that order.
    let private traverse
        (limits : PathLimits)
        (vfs : VirtualFileSystem)
        (directory : InodeNumber)
        (linkTarget : SymlinkTarget)
        (rest : PathCursor)
        (symlinks : int)
        : Result<InodeNumber * PathCursor, UnixError>
        =
        // Checked *before* the traversal happens, so a limit of 32 means
        // the 33rd attempt is the one that fails, matching both probes
        // (macOS resolves a chain of 32 and gives ELOOP at 33; Linux
        // resolves 40 and gives ELOOP at 41).
        //
        // This is also the bound that makes the walk terminate, which is
        // why there is no cycle detection: a seen-state set would *not*
        // be sufficient, because a link whose target names itself with a
        // suffix ("l" with target "l/x") grows the pathname buffer
        // forever without ever repeating a state. Only a count stops
        // that — and both real kernels use only a counter too, so this
        // is a transcription of their behaviour rather than an
        // approximation of it.
        if symlinks + 1 > PathLimits.maxSymlinkTraversals limits then
            Error UnixError.ELOOP
        else if

            // *After* the traversal count, because that is the order a
            // kernel checks them in and the two disagree. Measured on
            // Darwin: a chain whose last link both exhausts the budget and
            // would overflow the length reports ELOOP, while the same chain
            // one link shorter reports ENAMETOOLONG. XNU tests
            // `ni_loopcnt` in `namei` before it ever reads the target.
            //
            // Before the splice rather than after, so an overflowing
            // expansion is refused rather than performed — and note this
            // sees `rest`, whose cursor already sits past the separator run
            // the kernel collapsed, which is the whole reason the walk
            // carries a cursor.
            not (PathLimits.spliceWithinLimit limits linkTarget rest)
        then
            Error UnixError.ENAMETOOLONG
        else

        let linkPath = SymlinkTarget.toUnixPath linkTarget

        let next = if UnixPath.isRooted linkPath then vfs.Root else directory

        // Exactly what a kernel does to its pathname buffer: the target,
        // then whatever was left to resolve. Note this consumes `rest`,
        // whose cursor already sits past the separator run the kernel
        // collapsed — so the spliced buffer holds the same bytes the
        // kernel's would.
        Ok (next, PathCursor.splice linkPath rest)

    /// Consume components from `remaining` until the final name is in hand, or
    /// until the path runs out of names entirely.
    ///
    /// Every check the *prefix* of a path can fail — the holding directory's
    /// search bit, a non-final component's length, a non-final symlink's
    /// traversal budget and splice length — happens here. The final name's own
    /// length and lookup do not: they belong to `completeResolution`, and the
    /// split is what `PausedResolution` exists to express.
    ///
    /// The search bit is checked for the final component too, before this
    /// pauses. That is not an artefact of where the split fell: Linux's
    /// `link_path_walk` calls `may_lookup` at the top of each iteration, before
    /// it discovers whether the component it is about to hash is the last one,
    /// and `rename(unsearchable/f, absent/x)` is EACCES rather than ENOENT on
    /// both kernels.
    let rec private walkFrom
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (vfs : VirtualFileSystem)
        (directory : InodeNumber)
        (remaining : PathCursor)
        (trailing : bool)
        (finalSymlinkFollowed : bool)
        (lastNavigation : FinalNavigation)
        (symlinks : int)
        : Result<PausedResolution, UnixError>
        =
        let paused (final : (DirectoryEntryName * PathCursor) option) : PausedResolution =
            {
                Limits = limits
                Privilege = privilege
                Policy = policy
                TrailingSeparator = trailingSeparatorPolicy
                FileSystem = vfs
                Directory = directory
                Final = final
                Trailing = trailing
                FinalSymlinkFollowed = finalSymlinkFollowed
                LastNavigation = lastNavigation
                SymlinksTraversed = symlinks
            }

        match PathCursor.next remaining with
        // Reached when the path has no name left to look up: after a "." or
        // "..", or immediately for a path that named no component at all.
        //
        // Deliberately *before* the search check below, and that is what
        // makes `lstat("p")` and `lstat("p/")` succeed on an unsearchable
        // `p`: this walk never looks inside it. Measured on both kernels,
        // and `lstat("p/.")` is EACCES beside them -- one more place where
        // a trailing separator is not "/." and must not be desugared into
        // one.
        | None -> Ok (paused None)
        | Some (nextComponent, rest) ->

        // Consuming *any* component from `directory` -- a name, "." or ".."
        // alike -- is a lookup, and a lookup needs the directory's search
        // bit. Checked here, above the dispatch on which kind of component
        // it is, because all three need it: measured on both kernels,
        // `lstat("p/.")` and `lstat("p/..")` are EACCES exactly as
        // `lstat("p/kid")` is.
        //
        // Everything else this walk can refuse sits below this point, which
        // is what reproduces the measured precedence without encoding it:
        // against an unsearchable holding directory, a missing name, an
        // over-long one, a symlink cycle and (on Linux) a creating open's
        // trailing separator all report EACCES rather than the ENOENT,
        // ENAMETOOLONG, ELOOP or EISDIR the same call earns under a
        // searchable one.
        //
        // Only the *owner* triple can ever apply, which is
        // `PermissionBits.deniedTo`'s contract: `stat` reports `Kernel.UserId`
        // as every inode's `st_uid`, so the emulated process owns everything it
        // can see. Measured, and a corpus of ordinary modes cannot show it: a
        // 0o677 directory is EACCES to its owner though group and other may
        // search it, while 0o100 is searchable though nobody else may.
        let directoryContent =
            match VirtualFileSystem.tryGetDirectory directory vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: about to consume a component from inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

        if PermissionBits.deniedTo privilege AccessRequest.SearchDirectory directoryContent.Permissions then
            Error UnixError.EACCES
        else

        match nextComponent with
        | PathComponent.Current ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                directory
                rest
                trailing
                finalSymlinkFollowed
                FinalNavigation.Current
                symlinks
        | PathComponent.Parent ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                directoryContent.Parent
                rest
                trailing
                finalSymlinkFollowed
                FinalNavigation.Parent
                symlinks
        | PathComponent.Name name ->

        // The final name's length is checked by `completeResolution` rather
        // than here, and that is measured rather than tidy: on Linux
        // `rename("<300 bytes>", "nodir/x")` is ENOENT, so the source's final
        // component is still unmeasured when the destination's parent walk
        // fails. A non-final component's length is checked here, in walk
        // order, which is what makes "<300 bytes>/x" ENAMETOOLONG while
        // "nxdir/<300 bytes>" is ENOENT.
        if PathCursor.isExhausted rest then
            Ok (paused (Some (name, rest)))
        else if

            not (PathLimits.nameWithinLimit limits name)
        then
            Error UnixError.ENAMETOOLONG
        else

        match Map.tryFind name directoryContent.Entries with
        | None -> Error UnixError.ENOENT
        | Some target ->

        let content =
            match VirtualFileSystem.tryGetContent target vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: directory inode %O{directory} binds \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        match content with
        | InodeContent.Symlink linkTarget ->
            // A non-final symlink is always traversed, whatever the policy
            // says: `SymlinkPolicy.NoFollowFinal` is about the *final*
            // component alone, and no kernel offers a walk that stops at an
            // interior link.
            match traverse limits vfs directory linkTarget rest symlinks with
            | Error error -> Error error
            | Ok (next, spliced) ->

            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                next
                spliced
                trailing
                finalSymlinkFollowed
                // An interior link cannot leave the walk with nothing to
                // resolve — the remainder it was spliced in front of is
                // non-empty by construction — so, unlike the final case in
                // `completeResolution`, `lastNavigation` cannot become `Root`
                // here.
                lastNavigation
                (symlinks + 1)
        | InodeContent.Directory _ ->
            walkFrom
                limits
                privilege
                policy
                trailingSeparatorPolicy
                vfs
                target
                rest
                trailing
                finalSymlinkFollowed
                lastNavigation
                symlinks
        // A path cannot continue through a regular file.
        | InodeContent.RegularFile _ -> Error UnixError.ENOTDIR

    /// Whether the directory this paused resolution would look its final name up
    /// in has lost its own last name — so no path reaches it, though whatever
    /// holds it keeps it alive. False for a path that has no final name to look
    /// up, matching the verdicts, which ask this only of an `Entry`.
    ///
    /// Answered here rather than by handing out the directory, because "the
    /// directory the final name is looked up in" is settled only while the walk
    /// is paused: completing it can follow a final symlink and move it. The one
    /// caller is `rename`, which must ask *between* its two final lookups —
    /// measured, Linux reports an orphaned destination parent before it measures
    /// the destination's final name, and after it measures the source's.
    let pausedParentIsOrphaned (paused : PausedResolution) : bool =
        match box paused with
        | null ->
            failwith
                "PathWalk.pausedParentIsOrphaned: this paused resolution is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from PathWalk.resolveParent instead."
        | _ ->

        match paused.Final with
        | None -> false
        | Some _ -> VirtualFileSystem.isOrphanedDirectory paused.Directory paused.FileSystem

    /// Look the final name up, finishing the resolution `resolveParent` paused.
    ///
    /// Not simply "one `Map.tryFind`": under a policy that follows the final
    /// symlink, the lookup can splice a target into the pathname buffer and
    /// send the walk on to a different directory, so this re-enters the parent
    /// walk and finishes again.
    let rec completeResolution (paused : PausedResolution) : Result<Resolution, UnixError> =
        // The record is a reference type, so `Unchecked.defaultof` and C#
        // `default` give null rather than a record of zeroed fields — and a
        // field access on that reports a NullReferenceException from inside the
        // walk, naming nothing a caller could act on.
        match box paused with
        | null ->
            failwith
                "PathWalk.completeResolution: this paused resolution is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; obtain one from PathWalk.resolveParent instead."
        | _ -> ()

        let limits = paused.Limits
        let vfs = paused.FileSystem
        let directory = paused.Directory
        let trailing = paused.Trailing

        let finish (target : ResolvedTarget) : Result<Resolution, UnixError> =
            Ok
                {
                    Target = target
                    TrailingSeparatorDemanded = trailing
                    FinalSymlinkFollowed = paused.FinalSymlinkFollowed
                }

        match paused.Final with
        | None -> finish (ResolvedTarget.Directory (directory, paused.LastNavigation))
        | Some (name, rest) ->

        let directoryContent =
            match VirtualFileSystem.tryGetDirectory directory vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: about to look \"%s{DirectoryEntryName.toString name}\" up in inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

        // Before the length check, before the lookup, and before any symlink
        // this component names is traversed -- which is the whole content of
        // `RefuseIsDirectory`; see its docstring for the rows that pin the
        // position.
        match paused.TrailingSeparator with
        | TrailingSeparatorPolicy.RefuseIsDirectory when trailing -> Error UnixError.EISDIR
        | TrailingSeparatorPolicy.RefuseIsDirectory
        | TrailingSeparatorPolicy.Demand
        | TrailingSeparatorPolicy.Ignore ->

        // Before the lookup, and before anything notices whether the name
        // exists — which is what reproduces the measured precedence on both
        // kernels: "<300 bytes>/x" is ENAMETOOLONG (the over-long component
        // is reached and rejected) while "nxdir/<300 bytes>" is ENOENT (the
        // walk fails at the missing parent and never reaches it). Checking
        // after the lookup would report ENOENT for an over-long name that
        // does not exist, which is exactly what `stat` on a fresh long name
        // does *not* do.
        //
        // This is also the only place that sees components spliced in from a
        // symlink target, which a check at the syscall boundary could not.
        if not (PathLimits.nameWithinLimit limits name) then
            Error UnixError.ENAMETOOLONG
        else

        match Map.tryFind name directoryContent.Entries with
        | None ->
            // Not an error: the caller decides whether a free name is
            // ENOENT (`stat`) or the point of the call (`mkdir`). A
            // trailing separator does not change that — `mkdir("nx/")`
            // creates on both platforms.
            finish (ResolvedTarget.Entry (directory, name, None))
        | Some target ->

        let content =
            match VirtualFileSystem.tryGetContent target vfs with
            | Some content -> content
            | None ->
                failwith
                    $"VirtualFileSystem: directory inode %O{directory} binds \"%s{DirectoryEntryName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

        // A trailing separator forces the final symlink to be followed even
        // under NoFollowFinal: POSIX resolves "p/" as "p/.", and both
        // platforms agree for lookups (probed: `lstat("ld/")` stats the
        // directory the link names). Where they *disagree* is what a
        // mutating caller may then do, which is why the fact is reported
        // rather than acted on.
        //
        // `Ignore` is the one walk that opts out, because Linux's creating
        // lookup does: see that case for the rows.
        let trailingActsOnFinal =
            match paused.TrailingSeparator with
            | TrailingSeparatorPolicy.Demand
            | TrailingSeparatorPolicy.RefuseIsDirectory -> trailing
            | TrailingSeparatorPolicy.Ignore -> false

        let followFinal = paused.Policy = SymlinkPolicy.Follow || trailingActsOnFinal

        match content with
        | InodeContent.Symlink linkTarget when followFinal ->
            match traverse limits vfs directory linkTarget rest paused.SymlinksTraversed with
            | Error error -> Error error
            | Ok (next, spliced) ->

            // The link's own trailing separator only takes effect when
            // nothing follows it: when the walk has more to resolve, the
            // separator joining the target to the remainder absorbs it.
            //
            // It *adds to* the outer demand rather than replacing it. The
            // separator in "ld/" applies to whatever ld expands to, so a
            // link with target "d" still has to land on a directory; and a
            // link with target "d/" imposes the demand even when the
            // guest's own path carried none.
            //
            // This demand is threaded rather than read back off the spliced
            // buffer, and must be: resolving "ld/" consumes the trailing
            // separator into the cursor (that is what the kernel's own
            // collapse does), so the spliced buffer is just the target and
            // has forgotten it. A kernel remembers the same fact the same
            // way — XNU latches `TRAILINGSLASH` on the component rather than
            // re-reading the buffer.
            let trailing =
                trailing || UnixPath.hasTrailingSeparator (SymlinkTarget.toUnixPath linkTarget)

            // An empty splice can only mean the target was "/", that being
            // the one path with no components; the effective path is then
            // the root itself rather than whatever navigation preceded the
            // link.
            let lastNavigation =
                if PathCursor.isExhausted spliced then
                    FinalNavigation.Root
                else
                    paused.LastNavigation

            walkFrom
                limits
                paused.Privilege
                paused.Policy
                paused.TrailingSeparator
                vfs
                next
                spliced
                trailing
                true
                lastNavigation
                (paused.SymlinksTraversed + 1)
            |> Result.bind completeResolution
        | InodeContent.Symlink _ ->
            // Final position under NoFollowFinal with no trailing
            // separator: the link itself is the answer, which is what
            // `lstat` and `readlink` need.
            finish (ResolvedTarget.Entry (directory, name, Some target))
        | InodeContent.Directory _ -> finish (ResolvedTarget.Entry (directory, name, Some target))
        | InodeContent.RegularFile _ ->
            // "p/" where p exists and is not a directory is ENOTDIR --
            // for every lookup, on both platforms, and for Darwin's
            // `mkdir`. Linux's `mkdir` is the exception and answers
            // EEXIST, which is what `Ignore` selects: it never asks what
            // the final component was.
            if trailingActsOnFinal then
                Error UnixError.ENOTDIR
            else
                finish (ResolvedTarget.Entry (directory, name, Some target))

    /// Resolve `path` against `startDirectory`, which is where a *relative*
    /// path begins; a rooted path ignores it and starts at the root — stopping
    /// short of the final lookup, which `completeResolution` performs.
    ///
    /// Everything a path's *prefix* can be refused for happens here, and the
    /// final name's length and lookup happen there. That boundary is measured,
    /// not chosen for tidiness: see `PausedResolution` for the pair of
    /// `rename` rows that pin it.
    ///
    /// A trailing separator is deliberately *not* desugared into a "." component
    /// here, even though POSIX describes the two as equivalent. They are not,
    /// for anything that mutates: probed on macOS, `mkdir("d/")` succeeds while
    /// `mkdir("nx/.")` gives ENOENT, and `rmdir("d/")` succeeds while
    /// `rmdir("d/.")` gives EINVAL. Desugaring would also collapse the
    /// `Entry` that `mkdir("d/")` needs into a `Directory`. The demand is
    /// instead recorded on `Resolution` and enforced only where every platform
    /// agrees — except where `trailingSeparatorPolicy` says the walk must refuse
    /// it outright, which is a fact about the kernel rather than about the
    /// caller; see `TrailingSeparatorPolicy`.
    let resolveParent
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<PausedResolution, UnixError>
        =
        // Checked here rather than trusted, because this is the boundary a
        // forged value crosses: `create` refuses a zero limit, but a struct's
        // `Unchecked.defaultof` carries one anyway.
        let limits = PathLimits.assertValid "PathWalk.resolveParent" limits

        // POSIX gives the empty path ENOENT (probed on both). Walking zero
        // components would instead silently answer "the directory I started
        // from".
        if UnixPath.isEmpty path then
            Error UnixError.ENOENT
        else

        let start =
            if UnixPath.isRooted path then
                Ok vfs.Root
            else

            match VirtualFileSystem.tryGetContent startDirectory vfs with
            | None -> Error UnixError.ENOENT
            | Some (InodeContent.Directory _) -> Ok startDirectory
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR

        match start with
        | Error error -> Error error
        | Ok start ->

        walkFrom
            limits
            privilege
            policy
            trailingSeparatorPolicy
            vfs
            start
            (PathCursor.ofPath path)
            (UnixPath.hasTrailingSeparator path)
            false
            FinalNavigation.Root
            0

    /// Resolve `path` all the way: `resolveParent` followed by
    /// `completeResolution`, which is what every caller resolving a single path
    /// wants. Only a caller interleaving two resolutions needs the halves.
    let resolveFull
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (trailingSeparatorPolicy : TrailingSeparatorPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
        resolveParent limits privilege startDirectory policy trailingSeparatorPolicy path vfs
        |> Result.bind completeResolution

    /// `resolveFull`, discarding the how-it-finished facts. For the lookup
    /// operations, which are unanimous across platforms and so need none of
    /// them.
    let resolve
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<ResolvedTarget, UnixError>
        =
        resolveFull limits privilege startDirectory policy TrailingSeparatorPolicy.Demand path vfs
        |> Result.map (fun resolution -> resolution.Target)

    /// The inode a resolved target names. Turns a free final name into ENOENT,
    /// which is the one thing `resolve` deliberately does not do — so this is
    /// where "the file must already exist" is decided, once, for every caller
    /// that needs it.
    let existingOf (target : ResolvedTarget) : Result<InodeNumber, UnixError> =
        match target with
        | ResolvedTarget.Directory (inode, _) -> Ok inode
        | ResolvedTarget.Entry (_, _, Some inode) -> Ok inode
        | ResolvedTarget.Entry (_, _, None) -> Error UnixError.ENOENT

    /// The inode a path names, which is what `stat` and a non-creating `open`
    /// want: `resolve` followed by `existingOf`.
    let resolveExisting
        (limits : PathLimits)
        (privilege : CallerPrivilege)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber, UnixError>
        =
        resolve limits privilege startDirectory policy path vfs
        |> Result.bind existingOf

// ------------------------------------------------------------ inspection
