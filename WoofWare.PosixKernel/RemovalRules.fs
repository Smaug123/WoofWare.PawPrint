namespace WoofWare.PosixKernel

/// Why this kernel will not say what a sticky directory's rule does to an
/// `unlink(2)`, `rmdir(2)` or `rename(2)`: Darwin's answer for this caller has
/// not been measured.
///
/// Each case carries the sticky directory, the entry the call would remove or
/// replace, and how the caller stands towards that entry, which it owns
/// neither of.
[<RequireQualifiedAccess>]
type StickyRefusal =
    /// A privileged caller, on Darwin. Whether Darwin's sticky rule exempts
    /// root has not been measured: that needs root and a second user.
    | DarwinPrivilegedCaller of directory : InodeNumber * entry : InodeNumber * standing : Standing
    /// A `rename(2)` on Darwin in which a directory displaces the directory
    /// `entry`. Darwin consults the displaced directory's own write bit there
    /// rather than its parent's, and whether it consults the parent's sticky
    /// bit at all has not been measured.
    | DarwinDirectoryDisplacingDirectory of directory : InodeNumber * entry : InodeNumber * standing : Standing

/// Why this kernel will not answer an `unlink(2)` or an `rmdir(2)`.
[<RequireQualifiedAccess>]
type RemovalRefusal =
    /// Darwin's sticky rule has not been measured for this caller.
    | Sticky of StickyRefusal
    /// This kernel will not resolve the path.
    | Path of PathRefusal
    /// The call would remove `name` from `directory`, on the device
    /// filesystem, which no name can be removed from here: a real one would
    /// then answer for that name as it answers for any it does not hold, and
    /// this one refuses every name it does not hold.
    | DeviceFileSystem of directory : InodeNumber * name : DirectoryEntryName
    /// The path names `mountRoot`, the root of a mounted filesystem, in a
    /// sticky directory. The sticky rule consults the covered directory's
    /// owner, which this kernel does not hold.
    | MountPoint of mountRoot : InodeNumber

[<RequireQualifiedAccess>]
module StickyRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which paths.
    let describe (refusal : StickyRefusal) : string =
        match refusal with
        | StickyRefusal.DarwinPrivilegedCaller (directory, entry, standing) ->
            $"inode %O{directory} is a sticky directory, and the caller, standing %A{standing} towards its entry %O{entry}, owns neither of them but is privileged. Whether Darwin's sticky rule exempts a privileged caller has not been measured (it needs root and a second user)."
        | StickyRefusal.DarwinDirectoryDisplacingDirectory (directory, entry, standing) ->
            $"a directory would displace the directory %O{entry} in the sticky directory %O{directory}, and the caller, standing %A{standing} towards %O{entry}, owns neither of them. Darwin consults the displaced directory's own write bit there rather than its parent's, and whether it consults the sticky bit at all has not been measured."

[<RequireQualifiedAccess>]
module RemovalRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : RemovalRefusal) : string =
        match refusal with
        | RemovalRefusal.Sticky refusal -> StickyRefusal.describe refusal
        | RemovalRefusal.Path refusal -> PathRefusal.describe refusal
        | RemovalRefusal.MountPoint mountRoot ->
            $"the path names inode %O{mountRoot}, the root of a mounted filesystem, in a sticky directory. Whether the sticky rule lets the caller remove the name depends on the covered directory's owner, which this kernel does not hold."
        | RemovalRefusal.DeviceFileSystem (directory, name) ->
            $"the call would remove \"%s{DirectoryEntryName.toEscaped name}\" from inode %O{directory}, on the device filesystem, which holds only the nodes of the devices this kernel has drivers for; it removes none of them, because it could not then say what a real one answers for the name."

/// Which removal an `unlinkat(2)` makes, as its flag word says.
[<RequireQualifiedAccess>]
type RemovalKind =
    /// No `AT_REMOVEDIR`: the call is `unlink(2)`'s, under `UnlinkRules`.
    | Unlink
    /// `AT_REMOVEDIR`: the call is `rmdir(2)`'s, under `RmDirRules`.
    | RmDir

/// What screening `unlinkat(2)`'s flag word came to.
[<RequireQualifiedAccess>]
type UnlinkAtScreen =
    /// A word this kernel accepts, and the removal it asks for.
    | Screened of RemovalKind
    /// The call fails with this errno before its path is copied in.
    | Failed of error : UnixError
    /// The word carries `flags`, which the flavour accepts and this library
    /// does not model.
    | Unmodelled of flags : int

/// Why this kernel will not answer an `unlinkat(2)`.
[<RequireQualifiedAccess>]
type UnlinkAtRefusal =
    /// The flag word carries flags the flavour accepts and this library does
    /// not model; see `UnlinkAtScreen.Unmodelled`. `flags` is the whole word.
    | UnmodelledFlags of flags : int
    /// The flag word asked for a removal, and this kernel will not answer it.
    | Removal of RemovalRefusal

[<RequireQualifiedAccess>]
module UnlinkAtRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : UnlinkAtRefusal) : string =
        match refusal with
        | UnlinkAtRefusal.UnmodelledFlags flags ->
            $"the flag word 0x%x{flags} carries a flag Darwin accepts and this library does not model: 0x100, AT_SYMLINK_NOFOLLOW_ANY (0x800), 0x1000, AT_RESOLVE_BENEATH (0x2000), AT_NODELETEBUSY (0x4000) or AT_UNIQUE (0x8000)."
        | UnlinkAtRefusal.Removal refusal -> RemovalRefusal.describe refusal

[<RequireQualifiedAccess>]
module UnlinkAtRules =

    // `<fcntl.h>`'s numbering, measured by `at-dirfd.c` (CONST and FLAGS) on
    // Linux 6.18.5 and Darwin 27.0.
    let private linuxAtRemoveDir : int = 0x200
    let private darwinAtRemoveDir : int = 0x80
    // Two bits the SDK's <sys/fcntl.h> does not name, then
    // AT_SYMLINK_NOFOLLOW_ANY, AT_RESOLVE_BENEATH, AT_NODELETEBUSY and
    // AT_UNIQUE.
    let private darwinUnmodelledFlags : int =
        0x100 ||| 0x800 ||| 0x1000 ||| 0x2000 ||| 0x4000 ||| 0x8000

    /// `AT_REMOVEDIR` in `flavour`'s numbering: 0x200 on Linux, 0x80 on
    /// Darwin. `rmdir(2)` is `unlinkat(2)` with this flag from `AT_FDCWD`, and
    /// `unlink(2)` is it with no flags.
    let atRemoveDir (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxAtRemoveDir
        | SimulatedUnixFlavour.Darwin -> darwinAtRemoveDir

    /// Screen `unlinkat(2)`'s raw flag word as `flavour` does, before the path
    /// is copied in and before `dirfd` is looked at.
    ///
    /// Linux accepts only `AT_REMOVEDIR` (0x200). Darwin accepts
    /// `AT_REMOVEDIR` (0x80) and six flags this library does not model (see
    /// `UnlinkAtRefusal.UnmodelledFlags`). Each answers EINVAL for a word
    /// carrying any other bit, even beside a flag it accepts; a word whose
    /// bits Darwin accepts and which carries one of the six is
    /// `UnlinkAtScreen.Unmodelled`.
    let screen (flavour : SimulatedUnixFlavour) (flags : int) : UnlinkAtScreen =
        // Measured by `at-dirfd.c` (FLAGS, FLAGORDER: every single bit, and a
        // rejected bit against a NULL path, a bad dirfd and the empty path)
        // and by `unlinkat-rules.c` (FLAGS2: AT_REMOVEDIR, and each of
        // Darwin's six, beside a rejected bit, against a file, a directory, a
        // NULL path and a bad dirfd, which is EINVAL in every cell).
        let removeDir, unmodelled =
            match flavour with
            | SimulatedUnixFlavour.Linux -> linuxAtRemoveDir, 0
            | SimulatedUnixFlavour.Darwin -> darwinAtRemoveDir, darwinUnmodelledFlags

        if flags &&& ~~~(removeDir ||| unmodelled) <> 0 then
            UnlinkAtScreen.Failed UnixError.EINVAL
        elif flags &&& unmodelled <> 0 then
            UnlinkAtScreen.Unmodelled flags
        elif flags &&& removeDir <> 0 then
            UnlinkAtScreen.Screened RemovalKind.RmDir
        else
            UnlinkAtScreen.Screened RemovalKind.Unlink

/// <summary>
/// Parametrises the behaviour of different kernels when <c>unlink(2)</c> removes a name.
/// </summary>
/// <remarks>
/// Unlike e.g. <c>mkdir</c>, whose platform-dependent behaviour is confined entirely to
/// the directory walk, <c>unlink</c> diverges in the order of its refusals too.
///
/// Create one of these with <c>SimulatedUnixPlatform.unlinkRules</c>.
/// </remarks>
(*
Measured on macOS 26.6/APFS at uid 501 and 0, and Linux 6.x arm64 at uid
1000 and 0, one fresh tree per row.
*)
type UnlinkRules =
    {
        /// The walk `unlink` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms.
        ///
        /// Linux's `do_unlinkat` takes a parent and a name and never resolves
        /// the final component at all, so a trailing separator neither
        /// dereferences a final symlink nor is enforced by the walk: it is
        /// reported on `Resolution.TrailingSeparatorDemanded` and enforced by
        /// `linuxVerdict`. Darwin's `namei` resolves it like any other lookup,
        /// which is `Demand`.
        ///
        /// The row that separates them is `unlink("lroot/")` with `lroot -> "/"`:
        /// ENOTDIR on Linux, which cannot have traversed the link, against
        /// EISDIR on Darwin, which did.
        TrailingSeparator : TrailingSeparatorPolicy
    }

/// <summary>
/// What <c>unlink(2)</c> should do, now that its path has been resolved.
/// </summary>
[<RequireQualifiedAccess>]
type UnlinkVerdict =
    /// <summary>
    /// Answer the caller with this errno.
    /// </summary>
    | Refuse of error : UnixError
    /// <summary>
    /// Remove <c>name</c> from <c>directory</c>, and - if that was the last name the
    /// inode had and no open file description holds it - free the inode.
    /// </summary>
    /// <remarks>
    /// This doesn't carry the inode, so you should never store these for long enough
    /// that an inode-to-name mapping could become invalid.
    /// (WoofWare.PosixKernel's <c>unlink(2)</c> implementation uses the result straight
    /// away.)
    /// </remarks>
    | Remove of directory : InodeNumber * name : DirectoryEntryName

/// The two questions `unlink(2)` and `rmdir(2)` both ask about a name they have
/// been asked to remove. Neither is a policy: which of them is asked first, and
/// what a "yes" costs, is each syscall's own measured business.
[<RequireQualifiedAccess>]
module private RemovalChecks =
    /// The holding directory's inode and permission bits.
    ///
    /// Partial in `directory`, which the walk has just reported as the directory
    /// holding `name`.
    let private holding
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (vfs : VirtualFileSystem)
        : Inode * PermissionBits
        =
        match VirtualFileSystem.tryGet directory vfs with
        | Some parent -> parent, Inode.permissions parent
        | None ->
            failwith
                $"RemovalChecks.holding: resolution named inode %O{directory} as the directory holding \"%s{DirectoryEntryName.toEscaped name}\", but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

    /// Whether the *holding* directory refuses this caller the write bit it
    /// needs to remove a name from it.
    ///
    /// Write alone: the search half is the walk's, and a resolution that got
    /// this far has passed it. The sticky bit is a separate question, which
    /// `sticky` answers.
    ///
    /// Partial in `directory`, which the walk has just reported as the directory
    /// holding `name`.
    let lacksWrite
        (credentials : Credentials)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (vfs : VirtualFileSystem)
        : bool
        =
        // The lookup is above the privilege test, so its two assertions
        // fire for a privileged caller too. That is deliberate: both name a
        // corrupt inode graph, and root skipping the check would leave the
        // corruption to be found somewhere less informative.
        let parent, permissions = holding directory name vfs
        PermissionBits.deniedTo (Standing.toward credentials parent.Owner) AccessRequest.Write permissions

    /// What the holding directory's sticky bit says about this caller removing,
    /// renaming or replacing `name`, which is bound to `target`.
    ///
    /// Partial in `directory` and `target`, which the walk has just reported.
    let sticky
        (credentials : Credentials)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (target : InodeNumber)
        (vfs : VirtualFileSystem)
        : StickyRemoval
        =
        let parent, permissions = holding directory name vfs

        let entry =
            match VirtualFileSystem.tryGet target vfs with
            | Some entry -> entry
            | None ->
                failwith
                    $"RemovalChecks.sticky: the walk resolved \"%s{DirectoryEntryName.toEscaped name}\" to inode %O{target}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

        PermissionBits.stickyRemoval
            (Standing.toward credentials parent.Owner)
            (Standing.toward credentials entry.Owner)
            permissions

    /// How this caller stands towards `target`, which the walk has just
    /// reported.
    let standingTowards (credentials : Credentials) (target : InodeNumber) (vfs : VirtualFileSystem) : Standing =
        match VirtualFileSystem.tryGet target vfs with
        | Some entry -> Standing.toward credentials entry.Owner
        | None ->
            failwith
                $"RemovalChecks.standingTowards: the walk resolved a name to inode %O{target}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// Whether Darwin's sticky rule refuses this caller removing, renaming or
    /// replacing `name`, which is bound to `target`. Darwin refuses with EACCES,
    /// at the position of the write check (the two cannot be told apart). What
    /// it does for root has not been measured, so that is a refusal.
    let darwinStickyRefuses
        (credentials : Credentials)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (target : InodeNumber)
        (vfs : VirtualFileSystem)
        : Result<bool, StickyRefusal>
        =
        match sticky credentials directory name target vfs with
        | StickyRemoval.Unrestricted -> Ok false
        | StickyRemoval.Forbidden -> Ok true
        | StickyRemoval.ForbiddenButPrivileged ->
            Error (StickyRefusal.DarwinPrivilegedCaller (directory, target, standingTowards credentials target vfs))

    /// <summary>
    /// Whether the inode a name is bound to is a directory.
    /// </summary>
    /// <remarks>
    /// Throws if the supplied inode doesn't exist.
    /// </remarks>
    let isDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory _) -> true
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.CharacterDevice _)
        | Some (InodeContent.Symlink _) -> false
        | None ->
            failwith
                $"RemovalChecks.isDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

    /// <summary>
    /// Whether the directory at <c>inode</c> still holds an entry.
    /// </summary>
    /// <remarks>
    /// This is so that we can determine whether <c>rmdir(2)</c> should return
    /// <c>ENOTEMPTY</c>.
    ///
    /// "." and ".." are ignored for this check, just like real <c>rmdir</c> does.
    ///
    /// Throws if the supplied inode is a symlink or a regular file, or doesn't exist.
    /// </remarks>
    let isEmptyDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : bool =
        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Map.isEmpty directory.Entries
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.CharacterDevice _)
        | Some (InodeContent.Symlink _) ->
            failwith
                $"RemovalChecks.isEmptyDirectory: inode %O{inode} is not a directory, so it has no entries to count. Ask isDirectory first (this is a bug in the caller of RemovalChecks.isEmptyDirectory)."
        | None ->
            failwith
                $"RemovalChecks.isEmptyDirectory: the walk resolved a name to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."

[<RequireQualifiedAccess>]
module UnlinkRules =
    /// Linux's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component — "/", ".", "..", and any symlink
    ///    expansion of them — is EISDIR, whichever `FinalNavigation` it was and
    ///    whether or not the directory it reached is the root. Linux spends no
    ///    errno distinguishing them, where `rmdir` gives each its own (EBUSY,
    ///    EINVAL and ENOTEMPTY).
    ///  * A free final name is ENOENT, and that beats every check below:
    ///    `unlink("nowrite/nx/")` is ENOENT rather than the ENOTDIR the trailing
    ///    separator would earn or the EACCES the parent would.
    ///  * A trailing separator demands a directory, and reports what it found:
    ///    EISDIR for a directory, ENOTDIR for anything else. This is the arm
    ///    Linux's walk declines to make (`TrailingSeparatorPolicy.Ignore`), so
    ///    it never traverses a final symlink to get here — `unlink("ld/")`,
    ///    `unlink("dang/")`, `unlink("cyc/")` and `unlink("lroot/")` are all
    ///    ENOTDIR, with no ELOOP and no chance of destroying a link's target.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A sticky directory forbids removing an entry the caller owns neither
    ///    of: EPERM, unless the caller is privileged. Below the write check —
    ///    `unlink` of another user's file in an unwritable sticky directory is
    ///    EACCES — and above the directory arm: `unlink` of another user's
    ///    directory in a writable one is EPERM.
    ///  * The target being a directory is EISDIR — *below* the write check, and
    ///    measured to be: `unlink("nowrite/kdir")` is EACCES where
    ///    `unlink("nowrite/kdir/")` is EISDIR. That pair is the only thing
    ///    separating this arm from the trailing-separator one, since they share
    ///    an errno.
    ///
    /// EISDIR here is privilege-independent: measured at uid 0, Linux still
    /// refuses to `unlink` a directory. Privilege exempts the caller from the
    /// write bit and the sticky bit and from nothing else.
    let private linuxVerdict
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> UnlinkVerdict.Refuse UnixError.EISDIR
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if resolution.TrailingSeparatorDemanded then
            if RemovalChecks.isDirectory target vfs then
                UnlinkVerdict.Refuse UnixError.EISDIR
            else
                UnlinkVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite credentials directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        elif RemovalChecks.sticky credentials directory name target vfs = StickyRemoval.Forbidden then
            UnlinkVerdict.Refuse UnixError.EPERM
        elif RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EISDIR
        else
            UnlinkVerdict.Remove (directory, name)

    /// Darwin's `unlink(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU's `unlink1`
    ///    refusing a mount's root vnode (`vp->v_flag & VROOT`). This library
    ///    mounts no filesystem a Darwin path can reach (its devfs is refused),
    ///    so "the root of a mount" and "the root" are the same inode. Measured: `unlink("/.")`, `unlink("/..")` and — through
    ///    `lroot -> "/"` — `unlink("lroot/.")` are EBUSY, where `unlink("d/.")`
    ///    on an ordinary directory is EPERM.
    ///  * Any other directory reached with no final name is EPERM.
    ///  * A free final name is ENOENT.
    ///  * The target being a directory is EPERM, and beats the write check:
    ///    `unlink("nowrite/kdir")` is EPERM where `unlink("nowrite/kid")` is
    ///    EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A sticky directory forbids removing an entry the caller owns neither
    ///    of, and Darwin spends EACCES on it where Linux spends EPERM — so it is
    ///    indistinguishable from the write check, and below the directory arm
    ///    as that is: `unlink` of root's directory in root's writable sticky
    ///    directory is EPERM. Whether it exempts a privileged caller has not
    ///    been measured, and such a caller is refused.
    ///
    /// EPERM is privilege-independent — measured at uid 0, where `unlink("d")`
    /// is still EPERM and `rmdir("d")` succeeds. The `unlink(2)` man page's "and
    /// the effective user ID of the process is not the super-user" is stale
    /// relative to modern XNU, which refuses unconditionally.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so this function never
    /// sees `TrailingSeparatorDemanded` against a non-directory: the walk has
    /// already answered ENOTDIR (`unlink("f/")`, `unlink("lf/")`), ELOOP
    /// (`unlink("cyc/")`) or ENOENT (`unlink("dang/")`). What does reach here is
    /// a separator over a *directory*, whether named directly (`unlink("d/")`)
    /// or reached by following a final symlink (`unlink("ld/")`) — both EPERM,
    /// from the arm below, which is why the destructive divergence
    /// `Resolution.FinalSymlinkFollowed` warns about costs `unlink` nothing.
    let private darwinVerdict
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : Result<UnlinkVerdict, StickyRefusal>
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> Ok (UnlinkVerdict.Refuse UnixError.EISDIR)
            | FinalNavigation.Current
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    Ok (UnlinkVerdict.Refuse UnixError.EBUSY)
                else
                    Ok (UnlinkVerdict.Refuse UnixError.EPERM)
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> Ok (UnlinkVerdict.Refuse UnixError.ENOENT)
        | Some target ->

        if RemovalChecks.isDirectory target vfs then
            Ok (UnlinkVerdict.Refuse UnixError.EPERM)
        elif RemovalChecks.lacksWrite credentials directory name vfs then
            Ok (UnlinkVerdict.Refuse UnixError.EACCES)
        else

        match RemovalChecks.darwinStickyRefuses credentials directory name target vfs with
        | Error refusal -> Error refusal
        | Ok true -> Ok (UnlinkVerdict.Refuse UnixError.EACCES)
        | Ok false -> Ok (UnlinkVerdict.Remove (directory, name))

    /// <summary>
    /// Decide what <c>unlink(2)</c> does, given how its input path resolved.
    /// </summary>
    /// <remarks>
    /// Refused where Darwin's sticky rule has not been measured for this caller;
    /// see <c>StickyRefusal</c>.
    /// </remarks>
    let verdict
        (flavour : SimulatedUnixFlavour)
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : Result<UnlinkVerdict, StickyRefusal>
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> Ok (linuxVerdict credentials resolution vfs)
        | SimulatedUnixFlavour.Darwin -> darwinVerdict credentials resolution vfs

/// Everything a kernel does differently when `rmdir(2)` removes a directory.
///
/// Two fields, and the rest of the divergence — the *order* of the refusals and
/// the errno vocabulary — lives in `RmDirRules.linuxVerdict` and
/// `RmDirRules.darwinVerdict` rather than here, for the reason
/// `UnlinkRules.verdict` gives.
///
/// Measured on macOS 26.6/APFS at uid 501, and Linux 6.x arm64 at uid 1000 and
/// uid 0, one fresh tree per row.
type RmDirRules =
    {
        /// The walk `rmdir` resolves its path with, under
        /// `SymlinkPolicy.NoFollowFinal` on both platforms. Linux `Ignore`,
        /// Darwin `Demand`, exactly as `unlink`'s is and for the same reason.
        ///
        /// This is the field that makes the two flavours **destroy different
        /// objects**. With `ld -> d` and `d` an empty directory, `rmdir("ld/")`
        /// is ENOTDIR on Linux — whose walk cannot have traversed the link — and
        /// *removes `d`* on Darwin, whose walk did. It is the divergence
        /// `Resolution.FinalSymlinkFollowed` warns about, and the reason this
        /// syscall dispatches on the flavour rather than picking a column.
        TrailingSeparator : TrailingSeparatorPolicy
        /// What removing the directory does to the removed directory's own
        /// inode, which the flavours do not agree on.
        ///
        /// Measured through a descriptor held across the call, reproduced 3/3 on
        /// each: Linux drops the directory's `st_nlink` from 2 to 0 and moves its
        /// `ctime`, while Darwin leaves both alone. It is one fact, not two —
        /// nothing about the Darwin inode changed, so its `ctime` has no reason
        /// to move.
        ///
        /// Observable, which is why it is modelled rather than approximated:
        /// `fstat` on a directory descriptor reports `InodeTimes.StatusChange`
        /// as `ctime`. (`st_nlink` itself is not a `FileStatus` field, so only
        /// its shadow on `ctime` can be read.)
        ///
        /// `unlink` needs no such field: removing a *file*'s last name moves its
        /// `ctime` on both.
        RemovedDirectoryEffect : UnbindTargetEffect
    }

/// What `opendir(3)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type OpenDirVerdict =
    /// Answer the caller with this errno, and a NULL `DIR*`.
    | Refuse of error : UnixError
    /// Open a stream over this directory.
    | Open of directory : InodeNumber

[<RequireQualifiedAccess>]
module OpenDirRules =
    /// `opendir(3)`, transcribed from the measured ordering. Each arm beats the
    /// ones below it, and each bullet is a row measured on **both** kernels —
    /// there is no flavour parameter because there is no row they disagree on,
    /// which is why this takes none rather than defaulting one:
    ///
    ///  * A name nothing binds is ENOENT, and so is a dangling symlink: the walk
    ///    follows the final link, so there is nothing left to open.
    ///  * A target that is not a directory is ENOTDIR, and that beats the
    ///    permission check. The row proving it is a **mode-0000 regular file**,
    ///    which is ENOTDIR rather than EACCES — with and without a trailing
    ///    separator, and through a symlink to one. Pleasingly symmetric with
    ///    `open`'s own measured "EISDIR beats EACCES".
    ///  * A directory that refuses this caller the **read** bit is EACCES. Read,
    ///    not search, and this is the first place in this codebase where the two
    ///    come apart: a `0o111` directory (search, no read) is EACCES, while a
    ///    `0o444` one (read, no search) opens and lists every name. Search on the
    ///    *ancestors* is the walk's business and a resolution that got here has
    ///    passed it.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `opendir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// There is no root-navigation arm either, and `rmdir`'s three are the
    /// reason to say so rather than leave it implied: `opendir("/")`,
    /// `opendir("d/.")` and `opendir("d/..")` all simply succeed, on both.
    let verdict (credentials : Credentials) (resolution : Resolution) (vfs : VirtualFileSystem) : OpenDirVerdict =
        match PathWalk.existingOf resolution.Target with
        | Error error -> OpenDirVerdict.Refuse error
        | Ok inode ->

        match VirtualFileSystem.tryGet inode vfs with
        | None ->
            failwith
                $"OpenDirRules.verdict: the walk resolved to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        | Some {
                   Content = InodeContent.RegularFile _
               }
        | Some {
                   Content = InodeContent.CharacterDevice _
               }
        | Some {
                   Content = InodeContent.Symlink _
               } ->
            // The symlink arm is unreachable through the resolver, which
            // followed every final link and answered ENOENT for a dangling one.
            // It is the same answer either way, so there is nothing to refuse.
            OpenDirVerdict.Refuse UnixError.ENOTDIR
        | Some ({
                    Content = InodeContent.Directory content
                } as directory) ->

        if
            PermissionBits.deniedTo (Standing.toward credentials directory.Owner) AccessRequest.Read content.Permissions
        then
            OpenDirVerdict.Refuse UnixError.EACCES
        else
            OpenDirVerdict.Open inode

/// What `rmdir(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type RmDirVerdict =
    /// Answer the caller with this errno.
    | Refuse of error : UnixError
    /// Remove `name` from `directory`, and — since no other name can point at a
    /// directory — free the inode unless a descriptor or the current directory
    /// still holds it.
    ///
    /// Carries no inode for the reason `UnlinkVerdict.Remove` carries none: the
    /// removing code gets it from `VirtualFileSystem.unbind`, which answers the
    /// inode it actually unbound.
    | Remove of directory : InodeNumber * name : DirectoryEntryName

[<RequireQualifiedAccess>]
module RmDirRules =
    /// Linux's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it, and each bullet is a measured row:
    ///
    ///  * A path that consumed no component at all — "/" — is EBUSY. Linux
    ///    specialises the *path*, not the inode: `rmdir("/")` is EBUSY where
    ///    `rmdir("/.")` is EINVAL.
    ///  * A path whose last component was "." is EINVAL, whatever directory it
    ///    reached: `rmdir(".")`, `rmdir("d/.")` and `rmdir("/.")` all are.
    ///  * A path whose last component was ".." is ENOTEMPTY, again whatever it
    ///    reached. Not a coincidence with the emptiness check below — the parent
    ///    of any directory necessarily contains that directory — but it *is* a
    ///    separate arm, and the row proving it is `rmdir("nowrite/kdir/..")`,
    ///    which is ENOTEMPTY where the write check below would say EACCES.
    ///  * A free final name is ENOENT, and that beats the write check:
    ///    `rmdir("nowrite/nx")` is ENOENT.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A sticky directory forbids removing an entry the caller owns neither
    ///    of: EPERM, unless the caller is privileged. Below the write check,
    ///    as `unlink`'s is, and above both arms below: `rmdir` of another
    ///    user's regular file there is EPERM rather than ENOTDIR, and of
    ///    another user's non-empty directory EPERM rather than ENOTEMPTY.
    ///  * The target not being a directory is ENOTDIR — *below* the write check,
    ///    and measured to be: `rmdir("nowrite/kid")` is EACCES at uid 1000 and
    ///    ENOTDIR at uid 0. This is the arm Darwin orders the other way round.
    ///  * A directory a filesystem is mounted on is EBUSY, below the write
    ///    check: `rmdir("/dev")` is EACCES at uid 1000 and EBUSY at uid 0.
    ///  * A directory that still holds an entry is ENOTEMPTY.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `rmdir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// Measured at uid 0, every row: the EACCES and EPERM rows fall through to
    /// their next check and nothing else moves, so privilege exempts the caller
    /// from the write bit and the sticky bit and from nothing else.
    let private linuxVerdict
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (_, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EBUSY
            | FinalNavigation.Current -> RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent -> RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.lacksWrite credentials directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif RemovalChecks.sticky credentials directory name target vfs = StickyRemoval.Forbidden then
            RmDirVerdict.Refuse UnixError.EPERM
        elif not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif (VirtualFileSystem.mountOf target vfs).IsSome then
            RmDirVerdict.Refuse UnixError.EBUSY
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Darwin's `rmdir(2)`, transcribed from the measured ordering. Each arm
    /// beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", or a symlink whose
    ///    target was "/" — is EISDIR. Where Linux gives that path EBUSY.
    ///  * The root reached by "." or ".." is EBUSY, which is XNU refusing a
    ///    mount's root vnode; this library mounts no filesystem a Darwin path can
    ///    reach (its devfs is refused), so "the root of a mount" and "the root"
    ///    are the same inode. Measured: `rmdir("/.")`,
    ///    `rmdir("/..")` and — through `lroot -> "/"` — `rmdir("lroot/.")` are
    ///    EBUSY, where Linux answers those EINVAL and ENOTEMPTY. So Darwin
    ///    specialises the *inode* where Linux specialises the path.
    ///  * Any other directory reached by "." or ".." is EINVAL, and that
    ///    beats the write check: `rmdir("nowrite/kdir/..")` is EINVAL. Linux
    ///    agrees about "." and gives ".." ENOTEMPTY.
    ///  * A free final name is ENOENT.
    ///  * The target not being a directory is ENOTDIR, and beats the write
    ///    check: `rmdir("nowrite/kid")` is ENOTDIR where `rmdir("nowrite/kdir")`
    ///    is EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
    ///  * A sticky directory forbids removing an entry the caller owns neither
    ///    of, with EACCES, as `unlink`'s does on this flavour: below ENOTDIR
    ///    (`rmdir` of root's file in root's sticky directory is ENOTDIR) and
    ///    above ENOTEMPTY (of root's non-empty directory there, EACCES). A
    ///    privileged caller is refused, as `unlink` refuses one.
    ///  * A directory that still holds an entry is ENOTEMPTY, and the write
    ///    check beats it: `rmdir("nowrite/kfull")` is EACCES.
    ///
    /// Darwin's walk is `TrailingSeparatorPolicy.Demand`, so a separator over a
    /// non-directory never reaches here — the walk has already answered ENOTDIR
    /// (`rmdir("f/")`, `rmdir("lf/")`), ELOOP (`rmdir("cyc/")`) or ENOENT
    /// (`rmdir("dang/")`). What does reach here is a separator over a directory
    /// a final symlink named, and that is the destructive row: `rmdir("ld/")`
    /// removes `d`.
    let private darwinVerdict
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : Result<RmDirVerdict, StickyRefusal>
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> Ok (RmDirVerdict.Refuse UnixError.EISDIR)
            | FinalNavigation.Current
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    Ok (RmDirVerdict.Refuse UnixError.EBUSY)
                else
                    Ok (RmDirVerdict.Refuse UnixError.EINVAL)
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> Ok (RmDirVerdict.Refuse UnixError.ENOENT)
        | Some target ->

        if not (RemovalChecks.isDirectory target vfs) then
            Ok (RmDirVerdict.Refuse UnixError.ENOTDIR)
        elif RemovalChecks.lacksWrite credentials directory name vfs then
            Ok (RmDirVerdict.Refuse UnixError.EACCES)
        else

        match RemovalChecks.darwinStickyRefuses credentials directory name target vfs with
        | Error refusal -> Error refusal
        | Ok true -> Ok (RmDirVerdict.Refuse UnixError.EACCES)
        | Ok false ->

        if not (RemovalChecks.isEmptyDirectory target vfs) then
            Ok (RmDirVerdict.Refuse UnixError.ENOTEMPTY)
        else
            Ok (RmDirVerdict.Remove (directory, name))

    /// Decide what an `rmdir(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rmdir` makes the case more strongly than `unlink` did — the two flavours
    /// disagree about which of the root and the *path to it* is the special
    /// thing, which no table of errnos can express.
    ///
    /// Refused where Darwin's sticky rule has not been measured for this caller;
    /// see `StickyRefusal`.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : Result<RmDirVerdict, StickyRefusal>
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> Ok (linuxVerdict credentials resolution vfs)
        | SimulatedUnixFlavour.Darwin -> darwinVerdict credentials resolution vfs
