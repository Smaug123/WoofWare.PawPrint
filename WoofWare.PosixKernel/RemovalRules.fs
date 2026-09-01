namespace WoofWare.PosixKernel

/// <summary>
/// Parametrises the behaviour of different kernels when <c>unlink(2)</c> removes a name.
/// </summary>
/// <remarks>
/// Unlike e.g. <c>mkdir</c>, whose platform-dependent behaviour is confined entirely to
/// the directory walk, <c>unlink</c> diverges in the order of its refusals too.
///
/// Create one of these with <c>UnlinkRules.verdict</c>.
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
    /// Answer the guest with this errno.
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
    /// Whether the *holding* directory refuses this caller the write bit it
    /// needs to remove a name from it.
    ///
    /// Write alone: the search half is the walk's, and a resolution that got
    /// this far has passed it. Only the owner triple can ever apply, since
    /// `stat` reports `Kernel.UserId` as every inode's `st_uid`, and the sticky
    /// bit can never refuse for the same reason — POSIX permits the removal when
    /// the caller owns the file *or* the directory, and one kernel-wide identity
    /// owns both.
    ///
    /// Partial in `directory`, which the walk has just reported as the directory
    /// holding `name`.
    let lacksWrite
        (privilege : CallerPrivilege)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (vfs : VirtualFileSystem)
        : bool
        =
        // The lookup is above the privilege test, so its two assertions below
        // fire for a privileged caller too. That is deliberate: both name a
        // corrupt inode graph, and root skipping the check would leave the
        // corruption to be found somewhere less informative.
        let permissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match Inode.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"RemovalChecks.lacksWrite: the walk resolved \"%s{DirectoryEntryName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"RemovalChecks.lacksWrite: resolution named inode %O{directory} as the directory holding \"%s{DirectoryEntryName.toString name}\", but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        PermissionBits.deniedTo privilege AccessRequest.Write permissions

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
        | Some (InodeContent.Symlink _) ->
            failwith
                $"RemovalChecks.isEmptyDirectory: inode %O{inode} is not a directory, so it has no entries to count. Ask isDirectory first (this is an interpreter bug)."
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
    ///  * The target being a directory is EISDIR — *below* the write check, and
    ///    measured to be: `unlink("nowrite/kdir")` is EACCES where
    ///    `unlink("nowrite/kdir/")` is EISDIR. That pair is the only thing
    ///    separating this arm from the trailing-separator one, since they share
    ///    an errno.
    ///
    /// EISDIR here is privilege-independent: measured at uid 0, Linux still
    /// refuses to `unlink` a directory. `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
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
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
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
    ///    refusing a mount's root vnode (`vp->v_flag & VROOT`). PawPrint mounts
    ///    one filesystem, so "the root of a mount" and "the root" are the same
    ///    inode. Measured: `unlink("/.")`, `unlink("/..")` and — through
    ///    `lroot -> "/"` — `unlink("lroot/.")` are EBUSY, where `unlink("d/.")`
    ///    on an ordinary directory is EPERM.
    ///  * Any other directory reached with no final name is EPERM.
    ///  * A free final name is ENOENT.
    ///  * The target being a directory is EPERM, and beats the write check:
    ///    `unlink("nowrite/kdir")` is EPERM where `unlink("nowrite/kid")` is
    ///    EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
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
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> UnlinkVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    UnlinkVerdict.Refuse UnixError.EBUSY
                else
                    UnlinkVerdict.Refuse UnixError.EPERM
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> UnlinkVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if RemovalChecks.isDirectory target vfs then
            UnlinkVerdict.Refuse UnixError.EPERM
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            UnlinkVerdict.Refuse UnixError.EACCES
        else
            UnlinkVerdict.Remove (directory, name)

    /// <summary>
    /// Decide what <c>unlink(2)</c> does, given how its input path resolved.
    /// </summary>
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : UnlinkVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs

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
        /// Guest-observable, which is why it is modelled rather than approximated:
        /// `SystemNative_FStat` on a directory descriptor writes
        /// `InodeTimes.StatusChange` into `FileStatus`. (`st_nlink` itself is not
        /// a `FileStatus` field, so only its shadow on `ctime` can be read.)
        ///
        /// `unlink` needs no such field: removing a *file*'s last name moves its
        /// `ctime` on both.
        RemovedDirectoryEffect : UnbindTargetEffect
    }

/// What `opendir(3)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type OpenDirVerdict =
    /// Answer the guest with this errno, and a NULL `DIR*`.
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
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : OpenDirVerdict =
        match PathWalk.existingOf resolution.Target with
        | Error error -> OpenDirVerdict.Refuse error
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode vfs with
        | None ->
            failwith
                $"OpenDirRules.verdict: the walk resolved to inode %O{inode}, which the filesystem does not contain. Run VirtualFileSystem.checkInvariants."
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) ->
            // The symlink arm is unreachable through the resolver, which
            // followed every final link and answered ENOENT for a dangling one.
            // It is the same answer either way, so there is nothing to refuse.
            OpenDirVerdict.Refuse UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->

        if PermissionBits.deniedTo privilege AccessRequest.Read content.Permissions then
            OpenDirVerdict.Refuse UnixError.EACCES
        else
            OpenDirVerdict.Open inode

/// What `rmdir(2)` should do next, once its path has been resolved.
[<RequireQualifiedAccess>]
type RmDirVerdict =
    /// Answer the guest with this errno.
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
    ///  * The target not being a directory is ENOTDIR — *below* the write check,
    ///    and measured to be: `rmdir("nowrite/kid")` is EACCES at uid 1000 and
    ///    ENOTDIR at uid 0. This is the arm Darwin orders the other way round.
    ///  * A directory that still holds an entry is ENOTEMPTY.
    ///
    /// `Resolution.TrailingSeparatorDemanded` is never read, and does not need
    /// to be: the demand is "the final component must be a directory", which
    /// `rmdir` owes anyway. Measured, every `X/` row answers what its `X` row
    /// answers.
    ///
    /// Measured at uid 0, every row: the EACCES rows fall through to their next
    /// check and nothing else moves, so `CallerPrivilege` gates the write bit
    /// and nothing else.
    let private linuxVerdict
        (privilege : CallerPrivilege)
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

        if RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
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
    ///    mount's root vnode; PawPrint mounts one filesystem, so "the root of a
    ///    mount" and "the root" are the same inode. Measured: `rmdir("/.")`,
    ///    `rmdir("/..")` and — through `lroot -> "/"` — `rmdir("lroot/.")` are
    ///    EBUSY, where Linux answers those EINVAL and ENOTEMPTY. So Darwin
    ///    specialises the *inode* where Linux specialises the path.
    ///  * Any other directory reached by "." is EINVAL, and by ".." is
    ///    ENOTEMPTY — agreeing with Linux once the root is out of the way.
    ///  * A free final name is ENOENT.
    ///  * The target not being a directory is ENOTDIR, and beats the write
    ///    check: `rmdir("nowrite/kid")` is ENOTDIR where `rmdir("nowrite/kdir")`
    ///    is EACCES. This is the arm Linux orders the other way round.
    ///  * Removing a name needs write on the directory holding it: EACCES.
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
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match resolution.Target with
        | ResolvedTarget.Directory (inode, reachedBy) ->
            match reachedBy with
            | FinalNavigation.Root -> RmDirVerdict.Refuse UnixError.EISDIR
            | FinalNavigation.Current ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.EINVAL
            | FinalNavigation.Parent ->
                if inode = VirtualFileSystem.root vfs then
                    RmDirVerdict.Refuse UnixError.EBUSY
                else
                    RmDirVerdict.Refuse UnixError.ENOTEMPTY
        | ResolvedTarget.Entry (directory, name, existing) ->

        match existing with
        | None -> RmDirVerdict.Refuse UnixError.ENOENT
        | Some target ->

        if not (RemovalChecks.isDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTDIR
        elif RemovalChecks.lacksWrite privilege directory name vfs then
            RmDirVerdict.Refuse UnixError.EACCES
        elif not (RemovalChecks.isEmptyDirectory target vfs) then
            RmDirVerdict.Refuse UnixError.ENOTEMPTY
        else
            RmDirVerdict.Remove (directory, name)

    /// Decide what an `rmdir(2)` owes, given how its path resolved.
    ///
    /// Two whole functions rather than one reading a rules record, for the
    /// reason `UnlinkRules.verdict` states: what diverges is the order of the
    /// checks and the errno vocabulary rather than a constant they both consult.
    /// `rmdir` makes the case more strongly than `unlink` did — the two flavours
    /// disagree about which of the root and the *path to it* is the special
    /// thing, which no table of errnos can express.
    let verdict
        (flavour : SimulatedUnixFlavour)
        (privilege : CallerPrivilege)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : RmDirVerdict
        =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxVerdict privilege resolution vfs
        | SimulatedUnixFlavour.Darwin -> darwinVerdict privilege resolution vfs
