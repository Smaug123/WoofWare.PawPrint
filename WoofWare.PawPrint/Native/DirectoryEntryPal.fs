namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// What the PAL puts in `DirectoryEntry.NameLength`, which is a fact about the
/// libc it was compiled against rather than about any directory.
///
/// `ConvertDirent` (`pal_io.c:497`) copies `d_namlen` under
/// `HAVE_DIRENT_NAME_LEN` and writes `-1` otherwise, the sentinel meaning "walk
/// to the NUL yourself". Established by compiling rather than by reading:
/// glibc's `struct dirent` has no `d_namlen` member at all (`gcc` rejects
/// `d.d_namlen`), while macOS's `sys/dirent.h` declares one.
///
/// Invisible to managed code — `DirectoryEntry.GetName` takes
/// `CreateReadOnlySpanFromNullTerminated` for the sentinel and a plain span
/// otherwise — so only a guest that hand-rolls the P/Invoke can tell.
[<RequireQualifiedAccess>]
type DirectoryEntryNameLength =
    /// The name's length in bytes, as macOS reports it.
    | Reported
    /// `-1`, as every libc without `d_namlen` gets.
    | WalkToTerminator

/// The PAL's `DirectoryEntry` encoding of a directory entry, as
/// `SystemNative_ReadDir` writes it.
[<RequireQualifiedAccess>]
module DirectoryEntryPal =

    /// What this platform's PAL puts in `DirectoryEntry.NameLength`. See
    /// `DirectoryEntryNameLength`.
    let directoryEntryNameLength (platform : SimulatedUnixPlatform) : DirectoryEntryNameLength =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> DirectoryEntryNameLength.WalkToTerminator
        | SimulatedUnixFlavour.Darwin -> DirectoryEntryNameLength.Reported
