namespace WoofWare.PosixKernel

/// <summary>
/// Everything a kernel does differently when <c>open(2)</c> is asked to create.
/// </summary>
/// <remarks>
/// All these rules were measured on macOS 26.6/APFS and Linux 6.x, at an unprivileged uid.
/// </remarks>
type CreatingOpenRules =
    {
        /// What the walk owes a final component carrying a trailing separator.
        /// Linux refuses such a path outright; Darwin resolves it as any lookup
        /// would, so `open("d/", O_CREAT)` opens the directory there and is
        /// EISDIR on Linux.
        TrailingSeparator : TrailingSeparatorPolicy
        /// Whether a creating open that lands on an existing *directory* is
        /// refused. Linux answers EISDIR — so `open(dir, O_RDONLY|O_CREAT)`
        /// fails where a plain `open(dir, O_RDONLY)` succeeds — while Darwin
        /// treats `O_CREAT` as having no bearing on an object that exists.
        ///
        /// `O_EXCL`'s EEXIST is measured to beat this on both, so a caller must
        /// check that first.
        RefusesExistingDirectory : bool
        /// What a path that consumed *no component at all* — "/" itself, or a
        /// symlink whose target is "/" — owes a creating open.
        ///
        /// Darwin answers EEXIST even without `O_EXCL`; Linux folds the case
        /// into `RefusesExistingDirectory` and so wants `None` here. Pinned as a
        /// property of the *navigation* rather than of the root inode: on macOS
        /// "/" is EEXIST while "/.", "/../" and "/private/.." reach the same
        /// inode and open fine, and "/System/Volumes/Data" — a writable volume's
        /// mount root — opens fine too, which rules out a read-only-mount
        /// artefact.
        RootNavigation : UnixError option
        /// The bits `open(2)` keeps from its `mode` argument before the umask is
        /// applied. XNU masks with `ACCESSPERMS`, so a Darwin process cannot
        /// create a setuid, setgid or sticky file at all — measured, 0o4644,
        /// 0o2644 and 0o1644 all land as 0o644. Linux keeps all twelve bits.
        ModeMask : PermissionBits
        /// Whether a creating open that lands on an existing inode in a sticky
        /// directory is screened by who owns the two (see `CreationProtection`).
        /// Linux screens; its
        /// `fs.protected_*` sysctls decide only how far, and a symbolic link
        /// left unfollowed by `O_NOFOLLOW` is screened even with every one of
        /// them 0, so `open(l, O_CREAT|O_NOFOLLOW)` on another user's link in
        /// a world-writable sticky directory is EACCES rather than ELOOP.
        /// Darwin does not screen, and answers ELOOP there.
        ScreensStickyDirectoryEntries : bool
    }

/// What `open(2)` should do next, once the path has been resolved and the
/// creating flags have been read.
///
/// A verdict rather than an action, so the rule can be decided — and compared
/// against a real kernel — without a machine to act on it. `UnixNamespace.openPath`
/// is then only the part that acts: allocating the inode and registering a
/// descriptor.
[<RequireQualifiedAccess>]
type internal CreatingOpenVerdict =
    /// Answer the caller with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty regular file under `name` in `directory`.
    | Create of directory : InodeNumber * name : DirectoryEntryName

    /// The object is already there; open it, subject to the checks any
    /// non-creating open would apply.
    | OpenExisting of inode : InodeNumber

[<RequireQualifiedAccess>]
module internal CreatingOpenRules =
    /// Decide what an `open(2)` owes, given how its path resolved and whether it
    /// carried `O_CREAT` and `O_EXCL`.
    ///
    /// The order of the refusals is measured, and each beats the ones below it:
    ///
    ///  * `O_EXCL` on anything that exists is EEXIST — including a directory,
    ///    where it beats the EISDIR below: `open(".", O_CREAT|O_EXCL)` is EEXIST
    ///    while `open(".", O_CREAT)` is EISDIR on Linux.
    ///  * A *free* name that demands to be a directory creates nothing and is
    ///    ENOENT. Only Darwin reaches this: Linux refuses such a path inside the
    ///    walk, via `CreatingOpenRules.TrailingSeparator`.
    ///  * A path that consumed no component at all — "/" — is whatever
    ///    `RootNavigation` says, which is Darwin's EEXIST.
    ///  * A creating open landing on an existing directory is EISDIR on Linux.
    ///  * A creating open landing on any other existing inode, in a sticky
    ///    directory, is EACCES where `ScreensStickyDirectoryEntries` and
    ///    `ProtectedFiles.refusesCreatingOpen` under `protection` say so.
    ///    Measured on Linux after EEXIST and EISDIR, and before the `O_NOFOLLOW`
    ///    ELOOP, the permission check and any truncation.
    ///  * Binding a name needs the *write* bit on the directory that will hold
    ///    it, in the triple the caller's standing towards that directory
    ///    selects: measured as its owner at uid 1000, 0o333 and 0o300 succeed
    ///    while 0o644 and 0o555 are EACCES. Root bypasses it.
    ///
    ///    Binding needs the directory's *search* bit too — 0o111 is EACCES on
    ///    both kernels — but that half is not checked here: no resolution can
    ///    reach this function without it, because the walk refuses an
    ///    unsearchable directory before it looks a component up at all. See
    ///    `PathWalk.resolveFull`, which is also where the rows that
    ///    pin it live.
    ///  * Last, a name `bindable` does not admit is EILSEQ. Measured on Darwin
    ///    by `darwin-eilseq-is-last.c` in `docs/plans/2026-09-20-unix-path-bytes/`:
    ///    each refusal above beats it.
    ///
    /// Linux's `mknod(2)` of a regular file is decided by this verdict too,
    /// with `creating` and `exclusive` both set, after a walk of its own
    /// (`MkNodRules.verdict`).
    ///
    /// A freshly created inode is deliberately *not* screened against the mode
    /// it was just given — measured unanimously, `open(free, O_CREAT|O_RDWR, 0)`
    /// succeeds and stores mode 0, while re-opening that same file `O_RDONLY` is
    /// EACCES. That is why `Create` is a distinct verdict from `OpenExisting`
    /// rather than a step before it.
    let verdict
        (rules : CreatingOpenRules)
        (protection : ProtectedFiles)
        (bindable : BindableEntryNames)
        (credentials : Credentials)
        (creating : bool)
        (exclusive : bool)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : CreatingOpenVerdict
        =
        let existing = PathWalk.existingOf resolution.Target |> Result.toOption

        if not creating then
            match existing with
            | Some inode -> CreatingOpenVerdict.OpenExisting inode
            | None -> CreatingOpenVerdict.Refuse UnixError.ENOENT
        elif exclusive && existing.IsSome then
            CreatingOpenVerdict.Refuse UnixError.EEXIST
        else

        let isDirectory (inode : InodeNumber) : bool =
            match VirtualFileSystem.tryGetContent inode vfs with
            | Some (InodeContent.Directory _) -> true
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.CharacterDevice _)
            | Some (InodeContent.Symlink _)
            | None -> false

        match resolution.Target with
        | ResolvedTarget.Entry (_, _, None) when resolution.TrailingSeparatorDemanded ->
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        | ResolvedTarget.Directory (_, FinalNavigation.Root) when rules.RootNavigation.IsSome ->
            CreatingOpenVerdict.Refuse rules.RootNavigation.Value
        | ResolvedTarget.Directory (inode, _) ->
            if rules.RefusesExistingDirectory then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (directory, _, Some inode) ->
            if rules.RefusesExistingDirectory && isDirectory inode then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
            elif
                rules.ScreensStickyDirectoryEntries
                && (
                    match VirtualFileSystem.tryGet directory vfs, VirtualFileSystem.tryGet inode vfs with
                    | Some ({
                                Content = InodeContent.Directory parent
                            } as parentInode),
                      Some existing ->
                        ProtectedFiles.refusesCreatingOpen
                            protection
                            credentials
                            parentInode.Owner
                            parent.Permissions
                            existing
                    | _ ->
                        failwith
                            $"CreatingOpenRules.verdict: resolution named inode %O{inode} in directory inode %O{directory}, but the filesystem does not hold both, the second as a directory. Run VirtualFileSystem.checkInvariants."
                )
            then
                CreatingOpenVerdict.Refuse UnixError.EACCES
            else
                CreatingOpenVerdict.OpenExisting inode
        | ResolvedTarget.Entry (directory, name, None) ->

        // Nothing can be created inside a directory whose own last name has
        // gone: measured on both, `open("x", O_CREAT)` from inside an orphan is
        // ENOENT, at 0o755 and at 0o555 alike, so this beats the EACCES below.
        // `MkDirRules.verdict` states the same rule for the other creating
        // syscall.
        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            CreatingOpenVerdict.Refuse UnixError.ENOENT
        else

        // Write alone: the search half of the rule is the walk's, and a
        // resolution that reached here has already passed it.
        let parent, parentBits =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent -> parent, Inode.permissions parent
            | None ->
                failwith
                    $"CreatingOpenRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{DirectoryEntryName.toEscaped name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        if PermissionBits.deniedTo (Standing.toward credentials parent.Owner) AccessRequest.Write parentBits then
            CreatingOpenVerdict.Refuse UnixError.EACCES
        elif not (BindableEntryNames.admits bindable name) then
            CreatingOpenVerdict.Refuse UnixError.EILSEQ
        else
            CreatingOpenVerdict.Create (directory, name)

    /// <summary>
    /// The permission bits you get under <c>umask</c> when you create a file with this <c>mode</c>,
    /// in a directory carrying <c>parentPermissions</c> towards which the caller stands as
    /// <c>parentStanding</c>.
    /// </summary>
    /// <remarks>
    /// See <c>PermissionBits.fromCreationMode</c>, which is the general method to which <c>CreatingOpenRules</c>
    /// supplies platform-specific information.
    ///
    /// A requested <c>S_ISGID</c> is dropped when the file is created in a set-group-ID directory whose
    /// group the caller is not in, unless the caller is privileged, and only if the request also asked for
    /// <c>S_IXGRP</c>: a set-group-ID bit without group execute means mandatory locking rather than privilege.
    /// The request is judged before the umask is applied, so a umask that clears <c>S_IXGRP</c> does not
    /// save the bit.
    /// This matters only where <c>ModeMask</c> lets a caller request <c>S_ISGID</c> at all, which is Linux.
    /// </remarks>
    let createdPermissions
        (rules : CreatingOpenRules)
        (parentStanding : Standing)
        (parentPermissions : PermissionBits)
        (umask : PermissionBits)
        (mode : int)
        : PermissionBits
        =
        // Measured on Linux 6.18.5 (`permission-standing.c`, ext4 and tmpfs):
        // every one of the 4096 modes under umasks 0, 010, 022 and 07777, in a
        // 02777 parent of a group the creator is not in, of its supplementary
        // group and of its effective group, in a plain 0777 parent of a group it
        // is not in, and as root in the first. Umask 010 is the row that shows
        // the order: 02775 there gives 0765, where judging after the umask would
        // keep the bit.

        let requested = mode &&& PermissionBits.toInt rules.ModeMask

        let stripsSetGroupId =
            requested &&& (PermissionBits.setGroupId ||| PermissionBits.groupExecute) = (PermissionBits.setGroupId
                                                                                         ||| PermissionBits.groupExecute)
            && PermissionBits.toInt parentPermissions &&& PermissionBits.setGroupId <> 0
            && parentStanding.Privilege = CallerPrivilege.Unprivileged
            && not parentStanding.InGroup

        let mode =
            if stripsSetGroupId then
                mode &&& ~~~PermissionBits.setGroupId
            else
                mode

        PermissionBits.fromCreationMode rules.ModeMask umask mode
