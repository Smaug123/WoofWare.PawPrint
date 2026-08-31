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
    | Directory of entries : Map<DirectoryEntryName, SeedEntry> * permissions : PermissionBits
    /// Held verbatim, and *not* resolved when the seed is realised: a symlink's
    /// target is a string to the kernel, so it may dangle, may be absolute, and
    /// may point outside anything the seed declares.
    | Symlink of target : SymlinkTarget

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SeedEntry =
    /// <summary>
    /// What a <c>umask 022</c> process gets from <c>open(2)</c> called with <c>0o666</c>.
    /// </summary>
    let defaultPermsForRegularFile : PermissionBits =
        PermissionBits (0o666 &&& ~~~0o022)

    /// <summary>
    /// What a <c>umask 022</c> process inherits from <c>mkdir(2)</c>'s <c>0o777</c>.
    /// </summary>
    /// <remarks>
    /// <c>022</c> (-rw-r--r--) is the default umask on the emulated Unixes, although apparently Ubuntu
    /// relaxed this to <c>002</c> (-rw-rw-r--) starting at version 11.10.
    ///
    /// See <c>defaultForRegularFile</c> for the files version.
    /// </remarks>
    let defaultPermsForDirectory : PermissionBits = PermissionBits (0o777 &&& ~~~0o022)

    /// A regular file with the mode a `umask 022` process's `open(O_CREAT)`
    /// would have produced: 0666 &&& ~~~0o022.
    let file (contents : ImmutableArray<byte>) : SeedEntry =
        SeedEntry.File (contents, defaultPermsForRegularFile)

    /// A directory with the mode a `umask 022` process's `mkdir` would have
    /// produced: 0777 &&& ~~~0o022.
    let directory (entries : Map<DirectoryEntryName, SeedEntry>) : SeedEntry =
        SeedEntry.Directory (entries, defaultPermsForDirectory)

[<RequireQualifiedAccess>]
module FileSystemSeed =
    /// <summary>
    /// A seed describing nothing but an empty root directory.
    /// </summary>
    let empty : Map<DirectoryEntryName, SeedEntry> = Map.empty
