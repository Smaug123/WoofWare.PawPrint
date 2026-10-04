namespace WoofWare.PosixKernel

/// What `symlink(2)` does with an empty target.
[<RequireQualifiedAccess>]
type EmptySymlinkTarget =
    /// ENOENT, as soon as the target is copied in, before the link's own
    /// pathname is read. Linux.
    | NoSuchEntry
    /// Accepted: the call goes on, and if nothing else fails it creates a link
    /// that every walk through answers ENOENT. Darwin.
    | Accepted

/// Everything a flavour's `symlink(2)` does differently from the other's.
type SymlinkRules =
    {
        /// The walk `symlink` resolves the link's pathname with: Linux's is a
        /// plain lookup of the final name, so `symlink(t, "f/")` and
        /// `symlink(t, "dl/")` (dl dangling) are EEXIST there; Darwin's resolves
        /// a trailing separator as a lookup would, so the first is ENOTDIR and
        /// the second follows the link to its free target.
        TrailingSeparator : TrailingSeparatorPolicy
        /// What an empty target does.
        EmptyTarget : EmptySymlinkTarget
    }

/// What `symlink(2)` should do, now that the link's pathname has resolved.
[<RequireQualifiedAccess>]
type SymlinkVerdict =
    /// Answer the caller with this errno.
    | Refuse of error : UnixError
    /// Bind a new link under `name` in `directory`.
    | Create of directory : InodeNumber * name : DirectoryEntryName

[<RequireQualifiedAccess>]
module SymlinkRules =

    /// Decide what a `symlink(2)` owes, given how its link's pathname resolved.
    ///
    /// The flavours' differences are spent in the walk
    /// (`SymlinkRules.TrailingSeparator`) and before it
    /// (`SymlinkRules.EmptyTarget`); what reaches here is decided identically
    /// on both, except for the names `bindable` admits. In order, each beating
    /// those below it:
    ///
    ///  * a path that consumed no component ("/", ".", "..") is EEXIST;
    ///  * nothing can be created in a directory whose last name has gone:
    ///    ENOENT;
    ///  * an existing final name is EEXIST, whatever it is: a file, a
    ///    directory, or a link, dangling or not;
    ///  * a free final name with a trailing separator is ENOENT: unlike
    ///    `mkdir`, `symlink` never creates "n/";
    ///  * binding a name needs write on the directory that will hold it: EACCES;
    ///  * a name `bindable` does not admit is EILSEQ.
    let verdict
        (bindable : BindableEntryNames)
        (credentials : Credentials)
        (resolution : Resolution)
        (vfs : VirtualFileSystem)
        : SymlinkVerdict
        =
        // Measured by `link-symlink.c` (SYMLINK and SYMPERM) on Linux 6.18.5
        // and Darwin 27.0: "." is EEXIST; EEXIST and the trailing separator's
        // ENOENT each beat an unwritable directory's EACCES, and EACCES beats
        // Darwin's EILSEQ. `at-dirfd.c` measured the orphan's ENOENT.
        match resolution.Target with
        | ResolvedTarget.Directory _ -> SymlinkVerdict.Refuse UnixError.EEXIST
        | ResolvedTarget.Entry (directory, name, existing) ->

        if VirtualFileSystem.isOrphanedDirectory directory vfs then
            SymlinkVerdict.Refuse UnixError.ENOENT
        else

        match existing with
        | Some _ -> SymlinkVerdict.Refuse UnixError.EEXIST
        | None ->

        if resolution.TrailingSeparatorDemanded then
            SymlinkVerdict.Refuse UnixError.ENOENT
        else

        let parent =
            match VirtualFileSystem.tryGet directory vfs with
            | Some parent -> parent
            | None ->
                failwith
                    $"SymlinkRules.verdict: resolution named inode %O{directory} as the directory to create \"%s{DirectoryEntryName.toEscaped name}\" in, but the filesystem does not contain it. Run VirtualFileSystem.checkInvariants."

        if
            PermissionBits.deniedTo
                (Standing.toward credentials parent.Owner)
                AccessRequest.Write
                (Inode.permissions parent)
        then
            SymlinkVerdict.Refuse UnixError.EACCES
        elif not (BindableEntryNames.admits bindable name) then
            SymlinkVerdict.Refuse UnixError.EILSEQ
        else
            SymlinkVerdict.Create (directory, name)
