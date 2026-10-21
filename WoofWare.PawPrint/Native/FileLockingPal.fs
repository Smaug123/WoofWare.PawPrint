namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// What .NET 11's `SystemNative_FileSystemSupportsLocking` (`pal_io.c`) answers: whether
/// `SafeFileHandle` may take the `flock(2)` that carries a `FileShare` on a descriptor.
///
/// It refuses only a shared lock taken under write access on NFS, CIFS or SMB, where such a lock
/// drops writes silently (dotnet/runtime#44546 and #53182), and asks `fstatfs(2)` only for that
/// combination. .NET 10 made the same decision in managed code over `SystemNative_GetFileSystemType`
/// (see `FileSystemTypePal`); each flavour's half of the native reads the fields its own
/// `fstatfs(2)` fills in.
[<RequireQualifiedAccess>]
module FileLockingPal =

    /// `PAL_LOCK_EX` (`pal_io.h`), the exclusive lock, which `flock(2)` calls `LOCK_EX`.
    [<Literal>]
    let PalLockEx = 2

    /// The Linux `f_type` magic numbers on which the native refuses: `NFS_SUPER_MAGIC`,
    /// `CIFS_SUPER_MAGIC`, `SMB_SUPER_MAGIC` and `SMB2_SUPER_MAGIC`, in upstream's order.
    let linuxRefusedMagicNumbers : uint32 list =
        [ 0x6969u ; 0xFF534D42u ; 0x517Bu ; 0xFE534D42u ]

    /// The Darwin `f_fstypename`s on which the native refuses (`FileSystemNameSupportsLocking`), in
    /// upstream's order.
    let darwinRefusedNames : string list = [ "nfs" ; "cifs" ; "smb" ; "smb2" ]

    /// Whether the native asks `fstatfs(2)` for a lock of `lockOperation` taken with `accessWrite`
    /// (zero for none). Where it does not, it answers that locking is supported.
    let asksFileSystem (lockOperation : int32) (accessWrite : int32) : bool =
        // Upstream asserts that `lockOperation` is `PAL_LOCK_SH` or `PAL_LOCK_EX`, in a debug build
        // alone; a shipped shim treats any other value as it treats `PAL_LOCK_SH`, and so does this.
        lockOperation <> PalLockEx && accessWrite <> 0

    /// What the native answers once `fstatfs(2)` has reported `fields`: whether a shared lock under
    /// write access is safe on that filesystem.
    let supportsLockingOn (fields : FileSystemTypeFields) : bool =
        match fields with
        // `(unsigned int)statfsArgs.f_type`, the truncation of a `long`.
        | FileSystemTypeFields.Linux fType -> not (List.contains (uint32 fType) linuxRefusedMagicNumbers)
        // Darwin's half reads `f_fstypename` alone.
        | FileSystemTypeFields.Darwin (_, name) ->
            darwinRefusedNames
            |> List.exists (fun refused ->
                match UnixByteString.ofString refused with
                | Ok refused -> refused = name
                | Error defect -> failwith $"FileLockingPal: %s{refused} is not a filesystem name: %O{defect}"
            )
            |> not
