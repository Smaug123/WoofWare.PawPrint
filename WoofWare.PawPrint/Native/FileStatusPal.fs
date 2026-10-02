namespace WoofWare.PawPrint

/// The shim's `FileStatus.UserFlags` encoding of a file's BSD flags, and the
/// conversion `ConvertFileStatus` (`pal_io.c`) performs into it.
///
/// The library states `st_flags` as the kernel reports it, or `None` on a
/// flavour whose `struct stat` has no such field. The shim keeps one bit of
/// it: `PAL_UF_HIDDEN` when Darwin's `UF_HIDDEN` is set, and 0 for everything
/// else, including every Linux file, where the shim is built without
/// `HAVE_STAT_FLAGS`.
///
/// A transcription, so the compiler cannot keep it correct. Its oracles are the
/// pinned `pal_io.h`, which `TestFileStatusPal` reads `PAL_UF_HIDDEN` from, and
/// on a Darwin host the host's own `SystemNative_LStat`, which it compares
/// against this for files whose flags it has set.
[<RequireQualifiedAccess>]
module FileStatusPal =

    /// Darwin's `UF_HIDDEN`, from `<sys/stat.h>`.
    let private darwinHidden : uint32 = 0x8000u

    /// `PAL_UF_HIDDEN`, from `pal_io.h`.
    let palHidden : uint32 = 0x8000u

    /// The `UserFlags` a successful `SystemNative_Stat`, `LStat` or `FStat`
    /// writes for a file whose `st_flags` the kernel reported as `flags`.
    let userFlags (flags : uint32 option) : uint32 =
        match flags with
        | Some flags when flags &&& darwinHidden = darwinHidden -> palHidden
        | Some _
        | None -> 0u
