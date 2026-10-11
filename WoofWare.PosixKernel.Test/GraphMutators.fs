namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open WoofWare.PosixKernel

/// The `VirtualFileSystem` mutators, each answering the graph after it and
/// dropping the changes it reports: for tests about the graph alone.
/// `TestFsChange` is where the changes are tested.
[<RequireQualifiedAccess>]
module internal GraphMutators =

    let createDirectory
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.createDirectory directory name permissions owner now vfs
        |> Result.map (fun (inode, mutation) -> inode, VfsMutation.after mutation)

    let createFile
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (contents : ImmutableArray<byte>)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.createFile directory name permissions owner now contents vfs
        |> Result.map (fun (inode, mutation) -> inode, VfsMutation.after mutation)

    let cloneFile
        (source : InodeNumber)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.cloneFile source directory name permissions owner now vfs
        |> Result.map (fun (inode, mutation) -> inode, VfsMutation.after mutation)

    let createSymlink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (permissions : PermissionBits)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (target : SymlinkTarget)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.createSymlink directory name permissions owner now target vfs
        |> Result.map (fun (inode, mutation) -> inode, VfsMutation.after mutation)

    let hardLink
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (target : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.hardLink directory name target now vfs
        |> Result.map VfsMutation.after

    let unbind
        (effect : UnbindTargetEffect)
        (directory : InodeNumber)
        (name : DirectoryEntryName)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.unbind effect directory name now vfs
        |> Result.map (fun (inode, mutation) -> inode, VfsMutation.after mutation)

    let rename
        (sourceDirectory : InodeNumber)
        (sourceName : DirectoryEntryName)
        (destinationDirectory : InodeNumber)
        (destinationName : DirectoryEntryName)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<RenameOutcome * VirtualFileSystem, UnixError>
        =
        VirtualFileSystem.rename sourceDirectory sourceName destinationDirectory destinationName now vfs
        |> Result.map (fun (outcome, mutation) -> outcome, VfsMutation.after mutation)

    let forget (inode : InodeNumber) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.forget inode vfs |> VfsMutation.after

    let writeFile
        (inode : InodeNumber)
        (offset : int64)
        (bytes : ImmutableArray<byte>)
        (rule : SetGroupIdOnWrite)
        (credentials : Credentials)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, FileWriteRefusal>
        =
        VirtualFileSystem.writeFile inode offset bytes rule credentials now vfs
        |> Result.map VfsMutation.after

    let truncateFile
        (inode : InodeNumber)
        (length : int64)
        (rule : SetIdBitsOnTruncation)
        (credentials : Credentials)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, FileTruncationRefusal>
        =
        VirtualFileSystem.truncateFile inode length rule credentials now vfs
        |> Result.map VfsMutation.after

    let setPermissions
        (inode : InodeNumber)
        (bits : PermissionBits)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        VirtualFileSystem.setPermissions inode bits now vfs |> VfsMutation.after

    let setOwner
        (inode : InodeNumber)
        (owner : InodeOwner)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : VirtualFileSystem
        =
        VirtualFileSystem.setOwner inode owner now vfs |> VfsMutation.after

    let setTimes (inode : InodeNumber) (times : InodeTimes) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.setTimes inode times vfs |> VfsMutation.after
