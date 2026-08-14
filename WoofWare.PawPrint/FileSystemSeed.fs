namespace WoofWare.PawPrint

open System.Collections.Immutable

/// One entry in the filesystem a host hands to the emulated kernel.
///
/// A *tree*, rather than the list of absolute paths a manifest format would
/// use, and deliberately so: with the children of a directory held in its own
/// `Map`, "the same path declared twice", "a child declared before its parent"
/// and "the order of the declarations matters" are all unrepresentable rather
/// than checked. What is left to go wrong — a name that is not a legal
/// `FileName`, a symlink target that is not a legal `SymlinkTarget` — is
/// already refused by those types' own constructors, so a seed that type-checks
/// describes a filesystem, full stop.
///
/// The tree cannot express a hard link, since every inode has exactly one
/// container here. `VirtualFileSystem` can, so the day something needs a
/// seeded hard link this type grows a case; it is not a limit of the model.
///
/// Nor does it carry permission bits: every seeded inode gets
/// `PermissionBits.defaultForRegularFile` / `defaultForDirectory`, which is the
/// tree a `umask 022` process would have created. Nothing can observe the
/// difference yet — the only reader is `stat`, and `File.Exists` and
/// `Directory.Exists` consult solely the `S_IFMT` band — so a per-entry mode
/// would today be a knob no test could turn.
[<RequireQualifiedAccess>]
type SeedEntry =
    | File of contents : ImmutableArray<byte>
    | Directory of entries : Map<FileName, SeedEntry>
    /// Held verbatim, and *not* resolved when the seed is realised: a symlink's
    /// target is a string to the kernel, so it may dangle, may be absolute, and
    /// may point outside anything the seed declares.
    | Symlink of target : SymlinkTarget

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
    /// filesystem springs into existence at one instant, which is the honest
    /// story for an image that was never written to. Passed in rather than read
    /// from a clock: this file compiles before `EmulatedKernel`, and a
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
                    | SeedEntry.File contents ->
                        match
                            VirtualFileSystem.createFile
                                directory
                                name
                                PermissionBits.defaultForRegularFile
                                createdAt
                                contents
                                vfs
                        with
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
                    | SeedEntry.Directory children ->
                        match
                            VirtualFileSystem.createDirectory
                                directory
                                name
                                PermissionBits.defaultForDirectory
                                createdAt
                                vfs
                        with
                        | Ok (inode, vfs) -> install inode children vfs
                        | Error error ->
                            failwith
                                $"FileSystemSeed: could not create the directory %s{FileName.toString name}: %O{error}. Every name in a seed is unique within its directory by construction, so this cannot be a collision; the inode graph is inconsistent."
                )
                vfs

        let vfs = VirtualFileSystem.empty createdAt

        install (VirtualFileSystem.root vfs) entries vfs
        |> VirtualFileSystem.assertInvariants "FileSystemSeed.toVirtualFileSystem"
