namespace WoofWare.PosixKernel

/// <summary>
/// Everything a kernel does differently when <c>mkdir(2)</c> creates a directory.
/// </summary>
/// <remarks>
/// Measured at <c>umask 022</c> on macOS 25.6/APFS at uid 501 and Linux 6.x arm64 at
/// uid 1000.
/// </remarks>
type MkDirRules =
    {
        /// The walk `mkdir` resolves its path with. Linux's last component is a
        /// plain dentry lookup (`filename_create`), so a trailing separator buys
        /// nothing there and every existing final name is EEXIST; Darwin
        /// resolves it as a lookup would, which is how `mkdir("dang/")` creates
        /// the dangling link's *target* on that platform and answers ENOTDIR for
        /// "f/" and ELOOP for "cyc/".
        ///
        /// This field is why `MkDirRules.verdict` needs no rules: the divergence
        /// is spent inside the walk, and what comes out the other side is
        /// decided identically on both platforms.
        TrailingSeparator : TrailingSeparatorPolicy
        /// The bits `mkdir(2)` keeps from its `mode` argument before the umask
        /// is applied — which is *not* `CreatingOpenRules.ModeMask`. Linux keeps
        /// the sticky bit and drops both set-ID bits (`vfs_mkdir` masks with
        /// `S_IRWXUGO|S_ISVTX`), where its `open` keeps all twelve: measured,
        /// `mkdir(p, 0o7777)` gives 0o1755 and `mkdir(p, 0o2777)` gives 0o755.
        /// Darwin drops all three, as its `open` does.
        ModeMask : PermissionBits
        /// Whether a new directory inherits `S_ISGID` from the directory that
        /// holds it. Measured with a parent `chmod`ed to 0o2777 and read back at
        /// 0o2777 first: Linux gives the child 0o2755 from mode 0o777 and
        /// 0o3755 from 0o7777, so the bit is OR-ed in *after* both masks;
        /// Darwin gives 0o755 from every mode and does not inherit it at all.
        ///
        /// A kernel fact rather than a mount one on Linux — `inode_init_owner`
        /// (fs/inode.c) is VFS-generic, and a directory inherits the bit
        /// unconditionally when its parent carries it. The group-membership
        /// proviso beside it in that function applies only to non-directories,
        /// which is source-derived rather than measured, and is unobservable
        /// here anyway: PawPrint has one process-wide gid, so a new inode's
        /// group always matches its parent's. (`mount -o grpid` varies *gid*
        /// inheritance, not the bit, and one gid cannot see that either.)
        InheritsSetGroupIdFromParent : bool
    }

/// <summary>
/// What <c>mkdir(2)</c> should do, now that its path has been resolved.
/// </summary>
[<RequireQualifiedAccess>]
type MkDirVerdict =
    /// <summary>
    /// Answer the guest with this errno.
    /// </summary>
    | Refuse of error : UnixError
    /// <summary>
    /// Bind a new empty directory under <c>name</c> in <c>directory</c>.
    /// </summary>
    /// <remarks>
    /// The permission bits of the parent directory are carried with the verdict because
    /// the <c>S_ISGID</c> bit of the created directory depends on them.
    /// </remarks>
    | Create of directory : InodeNumber * name : DirectoryEntryName * parentPermissions : PermissionBits

[<RequireQualifiedAccess>]
module MkDirRules =
    /// Decide what a `mkdir(2)` owes, given how its path resolved.
    ///
    /// Takes no `MkDirRules`, and that is the point: every rule below is
    /// measured *identical* on both platforms. Everything `mkdir` diverges about
    /// is spent earlier, in the walk `MkDirRules.TrailingSeparator` selects, or
    /// later, in `createdPermissions`. Contrast `CreatingOpenRules.verdict`,
    /// which genuinely reads two of its fields.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * A path that consumed no component at all — "/", ".", ".." — is
    ///    EEXIST, whichever `FinalNavigation` it was. `mkdir` does not
    ///    distinguish them, where `rmdir` does: EBUSY, EINVAL and ENOTEMPTY on
    ///    Linux, and on Darwin EISDIR for the first with EBUSY swallowing the
    ///    other two at the root.
    ///  * An existing final name is EEXIST: a file, a directory, or a symlink,
    ///    dangling or cyclic or not.
    ///  * EEXIST beats the *write* bit. Measured on both: an existing child of a
    ///    0o555 directory is EEXIST, where a free name there is EACCES.
    ///  * Binding a new name needs write on the directory that will hold it:
    ///    measured, 0o333 and 0o300 succeed while 0o555 and 0o644 are EACCES.
    ///    Root bypasses it.
    ///
    /// The holding directory's *search* bit is needed as well — and needed
    /// earlier, since without it the final name cannot be looked up at all, so
    /// its absence beats even EEXIST. That check is the walk's
    /// (`PathWalk.resolveFull`), which refuses before this function is
    /// reached; the rows that pin it live there.
    ///
    /// A *free* final name carrying a trailing separator creates, on both
    /// platforms — `mkdir("nx/")` succeeds. This is the one place `mkdir` and a
    /// creating `open` disagree about a resolution of the same shape: `open`
    /// owes it ENOENT on Darwin.
    ///
    let verdict (privilege : CallerPrivilege) (resolution : Resolution) (vfs : VirtualFileSystem) : MkDirVerdict =
        match resolution.Target with
        | ResolvedTarget.Directory _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | ResolvedTarget.Entry (directory, name, existing) ->

        // Nothing can be created inside a directory whose own last name has
        // gone. Measured on both, at 0o755 and at 0o555, so ENOENT beats the
        // EACCES below; and `mkdir(".")` inside an orphan is still EEXIST, which
        // is why this sits under the `Directory` arm rather than above it.
        //
        // Above the `existing` match because that is where a real kernel puts
        // it: the ENOENT comes from the lookup itself failing against a dead
        // parent. The ordering is not observable — an orphan is necessarily
        // empty, since `rmdir` refuses a populated directory and this rule stops
        // one ever gaining an entry.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            MkDirVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it.
        let parentPermissions =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match Inode.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"MkDirRules.verdict: the walk resolved \"%s{DirectoryEntryName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"MkDirRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{DirectoryEntryName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        match existing with
        | Some _ -> MkDirVerdict.Refuse UnixError.EEXIST
        | None ->

        if PermissionBits.deniedTo privilege AccessRequest.Write parentPermissions then
            MkDirVerdict.Refuse UnixError.EACCES
        else
            MkDirVerdict.Create (directory, name, parentPermissions)

    /// The permission bits a directory created with this `mode` argument ends up
    /// with, inside a parent whose own bits are `parentPermissions`.
    ///
    /// `PermissionBits.fromCreationMode` under `MkDirRules.ModeMask`, then
    /// `S_ISGID` OR-ed in where the platform inherits it. The OR is last, and
    /// measured to be: Linux's `mkdir(sg, 0o7777)` in a 0o2777 parent gives
    /// 0o3755, so the bit survives a mask that would otherwise have cleared it.
    let createdPermissions
        (rules : MkDirRules)
        (parentPermissions : PermissionBits)
        (umask : PermissionBits)
        (mode : int)
        : PermissionBits
        =
        let setGroupId = 0o2000
        let masked = PermissionBits.fromCreationMode rules.ModeMask umask mode

        let inherited =
            rules.InheritsSetGroupIdFromParent
            && PermissionBits.toInt parentPermissions &&& setGroupId <> 0

        if inherited then
            PermissionBits.toInt masked ||| setGroupId
            |> PermissionBits.parseOrFail "MkDirRules.createdPermissions"
        else
            masked
