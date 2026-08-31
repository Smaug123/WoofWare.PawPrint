namespace WoofWare.PosixKernel

/// Everything a kernel does differently when `open(2)` is asked to *create*.
///
/// One record rather than a scatter of booleans, because the divergence is
/// several facts that always travel together: a platform that answers one of
/// them Linux's way answers all of them Linux's way, and a third Unix must
/// supply every field before it compiles. All four were measured on macOS
/// 26.6/APFS and Linux 6.x, at an unprivileged uid.
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
        /// applied. XNU masks with `ACCESSPERMS`, so a Darwin guest cannot
        /// create a setuid, setgid or sticky file at all — measured, 0o4644,
        /// 0o2644 and 0o1644 all land as 0o644. Linux keeps all twelve bits.
        ModeMask : PermissionBits
    }

/// What `open(2)` should do next, once the path has been resolved and the
/// creating flags have been read.
///
/// A verdict rather than an action, so the rule can be decided — and compared
/// against a real kernel — without a machine to act on it. The handler is then
/// only the part that cannot be pure: allocating the inode, registering a
/// descriptor and pushing the result.
[<RequireQualifiedAccess>]
type CreatingOpenVerdict =
    /// Answer the guest with this errno.
    | Refuse of error : UnixError
    /// Bind a new empty regular file under `name` in `directory`.
    | Create of directory : InodeNumber * name : DirectoryEntryName

    /// The object is already there; open it, subject to the checks any
    /// non-creating open would apply.
    | OpenExisting of inode : InodeNumber

[<RequireQualifiedAccess>]
module CreatingOpenRules =
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
    ///  * Binding a name needs the *write* bit on the directory that will hold
    ///    it: measured at uid 1000, 0o333 and 0o300 succeed while 0o644 and
    ///    0o555 are EACCES. Root bypasses it.
    ///
    ///    Binding needs the directory's *search* bit too — 0o111 is EACCES on
    ///    both kernels — but that half is not checked here: no resolution can
    ///    reach this function without it, because the walk refuses an
    ///    unsearchable directory before it looks a component up at all. See
    ///    `VirtualFileSystem.resolveFull`, which is also where the rows that
    ///    pin it live.
    ///
    /// A freshly created inode is deliberately *not* screened against the mode
    /// it was just given — measured unanimously, `open(free, O_CREAT|O_RDWR, 0)`
    /// succeeds and stores mode 0, while re-opening that same file `O_RDONLY` is
    /// EACCES. That is why `Create` is a distinct verdict from `OpenExisting`
    /// rather than a step before it.
    let verdict
        (rules : CreatingOpenRules)
        (privilege : CallerPrivilege)
        (creating : bool)
        (exclusive : bool)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : CreatingOpenVerdict
        =
        let existing = VirtualFileSystem.existingOf resolution.Target |> Result.toOption

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
        | ResolvedTarget.Entry (_, _, Some inode) ->
            if rules.RefusesExistingDirectory && isDirectory inode then
                CreatingOpenVerdict.Refuse UnixError.EISDIR
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
        let parentBits =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent ->
                match Inode.permissions parent with
                | InodePermissions.Stored bits -> bits
                | InodePermissions.PlatformSymlinkDefault ->
                    failwith
                        $"CreatingOpenRules.verdict: the walk resolved \"%s{DirectoryEntryName.toString name}\" inside inode %O{directory}, which reports platform-default symlink permissions -- but only a directory can hold an entry (this is an interpreter bug)."
            | None ->
                failwith
                    $"CreatingOpenRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{DirectoryEntryName.toString name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        if PermissionBits.deniedTo privilege AccessRequest.Write parentBits then
            CreatingOpenVerdict.Refuse UnixError.EACCES
        else
            CreatingOpenVerdict.Create (directory, name)

    /// The permission bits a file created with this `mode` argument ends up
    /// with, under `umask`. See `PermissionBits.fromCreationMode`, which states
    /// the rule once for every creating syscall; `ModeMask` is `open`'s half of
    /// it, and is how a Darwin guest cannot create a setuid file at all.
    let createdPermissions (rules : CreatingOpenRules) (umask : PermissionBits) (mode : int) : PermissionBits =
        PermissionBits.fromCreationMode rules.ModeMask umask mode
