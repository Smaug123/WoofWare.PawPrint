namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The BCL's `UnixFileSystemTypes` encoding of a filesystem's type, and the
/// conversion `SystemNative_GetFileSystemType` performs into it.
///
/// This is PawPrint's half of the `fstatfs` boundary. The library states the
/// fields a kernel's `fstatfs(2)` fills in; the shim reduces them to one
/// `uint32`, and does so differently per flavour (`pal_io.c`,
/// `SystemNative_GetFileSystemType`):
///
/// - on Linux it returns `f_type` cast to `uint32_t`, so the number is the
///   kernel's own magic number;
/// - on Darwin it ignores `f_type`, which its comment calls version-specific,
///   and looks `f_fstypename` up in `MapFileSystemNameToEnum`'s name table,
///   whose numbers are Linux's magic numbers wherever Linux has the filesystem.
///
/// CoreLib casts the result straight to `Interop.Sys.UnixFileSystemTypes`,
/// whose members carry the same numbers.
///
/// The name table is a transcription, so the compiler cannot keep it correct.
/// Its oracle is upstream: `TestFileSystemTypePal` re-derives each row from the
/// pinned `pal_io.c`.
[<RequireQualifiedAccess>]
module FileSystemTypePal =

    /// The rows of `MapFileSystemNameToEnum` for every name the library's
    /// Darwin can report, in upstream's order. Upstream has over a hundred rows;
    /// only these can be reached, because `EmulatedFileSystemType.fieldsFor`
    /// names no other Darwin filesystem.
    let darwinNameRows : (string * uint32) list = [ "apfs", 0x1Au ; "nfs", 0x6969u ]

    /// What a successful `SystemNative_GetFileSystemType` returns for a
    /// descriptor whose `fstatfs(2)` reported `fields`.
    ///
    /// Throws for a Darwin name `darwinNameRows` does not carry. Upstream
    /// would return 0 there (after a debug-build `assert`), which is also how
    /// it reports failure, so a guest cannot tell the two apart; that row has
    /// to be transcribed before PawPrint can answer for it.
    let ofFields (fields : FileSystemTypeFields) : uint32 =
        match fields with
        // `(uint32_t)statfsArgs.f_type`, the truncation of a `long`.
        | FileSystemTypeFields.Linux fType -> uint32 fType
        | FileSystemTypeFields.Darwin (_, name) ->
            let row =
                darwinNameRows
                |> List.tryFind (fun (candidate, _) ->
                    match UnixByteString.ofString candidate with
                    | Ok candidate -> candidate = name
                    | Error _ -> false
                )

            match row with
            | Some (_, value) -> value
            | None ->
                failwith
                    $"FileSystemTypePal.ofFields: a Darwin fstatfs reported the filesystem name %s{UnixByteString.toEscaped name}, which has no row in FileSystemTypePal.darwinNameRows. Transcribe its row from MapFileSystemNameToEnum in pal_io.c."
