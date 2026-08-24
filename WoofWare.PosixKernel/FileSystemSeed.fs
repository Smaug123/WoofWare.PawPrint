namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// One entry in the filesystem a host hands to the emulated kernel.
///
/// A tree, so duplicate paths, orphaned children, and declaration order are
/// unrepresentable rather than checked.
///
/// The tree cannot express a hard link, since every inode has exactly one
/// container here. `VirtualFileSystem` can, so a seeded hard link would mean
/// growing a case here; it is not a limit of the model.
///
/// A file's or directory's permission bits are part of the seed, and are
/// required rather than optional: the differential oracle chmods the host tree
/// to the same bits, which makes the mode a cross-runtime fact.
///
/// `SeedEntry.file` and `SeedEntry.directory` supply the modes a `umask 022`
/// process would have created, for the many seeds that only care about shape.
///
/// A symlink has no mode field, and must not: Linux ignores a symlink's own
/// mode entirely, `lchmod` is not portable, and `VirtualFileSystem` already
/// models this with `InodePermissions.PlatformSymlinkDefault`.
[<RequireQualifiedAccess>]
type SeedEntry =
    | File of contents : ImmutableArray<byte> * permissions : PermissionBits
    | Directory of entries : Map<FileName, SeedEntry> * permissions : PermissionBits
    /// Held verbatim, and *not* resolved when the seed is realised: a symlink's
    /// target is a string to the kernel, so it may dangle, may be absolute, and
    /// may point outside anything the seed declares.
    | Symlink of target : SymlinkTarget

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SeedEntry =
    /// A regular file with the mode a `umask 022` process's `open(O_CREAT)`
    /// would have produced: 0666 &&& ~~~0o022.
    let file (contents : ImmutableArray<byte>) : SeedEntry =
        SeedEntry.File (contents, PermissionBits.defaultForRegularFile)

    /// A directory with the mode a `umask 022` process's `mkdir` would have
    /// produced: 0777 &&& ~~~0o022.
    let directory (entries : Map<FileName, SeedEntry>) : SeedEntry =
        SeedEntry.Directory (entries, PermissionBits.defaultForDirectory)

[<RequireQualifiedAccess>]
module FileSystemSeed =
    /// A seed describing nothing but an empty root directory.
    let empty : Map<FileName, SeedEntry> = Map.empty

    /// Realise a seed as an inode graph whose root directory holds `entries`.
    ///
    /// The root is a `Map` rather than a `SeedEntry` because a filesystem's
    /// root is always a directory: taking the entries directly makes "the root
    /// is a regular file" unrepresentable instead of an error to report.
    ///
    /// `createdAt` is every seeded inode's birth, mtime, ctime and atime — the
    /// filesystem springs into existence at one instant. Passed in rather than
    /// read from a clock: this file compiles before `EmulatedKernel`, and a
    /// filesystem that read the host's clock would make a replay depend on when
    /// it was recorded.
    let toVirtualFileSystem (createdAt : UnixTimestamp) (entries : Map<FileName, SeedEntry>) : VirtualFileSystem =
        let rec install
            (directory : InodeNumber)
            (entries : Map<FileName, SeedEntry>)
            (vfs : VirtualFileSystem)
            : VirtualFileSystem
            =
            // `Map` iterates in key order, so the inode numbers a seed produces
            // are a function of the seed alone rather than of how the host
            // happened to build the map. Inode numbers are guest-observable
            // through `st_ino`, so this is part of the replay contract.
            entries
            |> Map.fold
                (fun vfs name entry ->
                    match entry with
                    | SeedEntry.File (contents, permissions) ->
                        match VirtualFileSystem.createFile directory name permissions createdAt contents vfs with
                        | Ok (_, vfs) -> vfs
                        | Error error ->
                            failwith
                                $"FileSystemSeed: could not create the file %s{FileName.toString name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                    | SeedEntry.Symlink target ->
                        match VirtualFileSystem.createSymlink directory name createdAt target vfs with
                        | Ok (_, vfs) -> vfs
                        | Error error ->
                            failwith
                                $"FileSystemSeed: could not create the symlink %s{FileName.toString name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                    | SeedEntry.Directory (children, permissions) ->
                        match VirtualFileSystem.createDirectory directory name permissions createdAt vfs with
                        | Ok (inode, vfs) -> install inode children vfs
                        | Error error ->
                            failwith
                                $"FileSystemSeed: could not create the directory %s{FileName.toString name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                )
                vfs

        let vfs = VirtualFileSystem.empty createdAt

        install (VirtualFileSystem.root vfs) entries vfs
        |> VirtualFileSystem.assertInvariants "FileSystemSeed.toVirtualFileSystem"
