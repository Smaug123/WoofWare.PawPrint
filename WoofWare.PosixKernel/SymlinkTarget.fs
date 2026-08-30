namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The target of a symbolic link, held exactly as it was created.
///
/// Verbatim rather than parsed: `readlink(2)` returns the stored
/// bytes unchanged, and `lstat` reports their length as the link's `st_size`, so
/// a link created with target "a//b/" must read back as "a//b/" — a difference a
/// guest really can see, through `FileInfo.LinkTarget` and `ResolveLinkTarget`.
///
/// The path structure is recovered by parsing at traversal time, which is
/// cheap, total, and keeps the stored form authoritative. `UnixPath` is kept
/// the same way and for a related reason (a kernel measures the bytes it was
/// handed), so converting a target to one loses nothing.
[<Struct>]
type SymlinkTarget =
    private
    | SymlinkTarget of target : string

    override this.ToString () : string =
        match this with
        | SymlinkTarget target -> target

/// Why a string is not usable as the target of a symbolic link.
[<RequireQualifiedAccess>]
type SymlinkTargetError =
    /// The candidate was null or empty. `symlink(2)` on Linux rejects an empty
    /// target with ENOENT — but macOS *accepts* it, creating a link that then
    /// fails to resolve. PawPrint refuses to represent one at all rather than
    /// picking a platform, so the divergence can only arise at the `symlink`
    /// boundary, never inside a seed manifest.
    | Empty
    /// The candidate could not survive the `char*` boundary; see
    /// `UnixPathTextDefect`.
    | Text of defect : UnixPathTextDefect

[<RequireQualifiedAccess>]
module SymlinkTarget =
    let toString (target : SymlinkTarget) : string =
        match target with
        | SymlinkTarget target -> target

    /// Parse a symlink target. Total: never throws, for any input including
    /// null.
    let parse (candidate : string) : Result<SymlinkTarget, SymlinkTargetError> =
        if System.String.IsNullOrEmpty candidate then
            Error SymlinkTargetError.Empty
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (SymlinkTargetError.Text defect)
        | None -> Ok (SymlinkTarget candidate)

    let describe (error : SymlinkTargetError) : string =
        match error with
        | SymlinkTargetError.Empty ->
            "symlink target is null or empty; Linux rejects that with ENOENT while macOS accepts it, so PawPrint declines to represent it"
        | SymlinkTargetError.Text defect -> $"symlink target %s{UnixPathText.describe defect}"

    let parseOrFail (context : string) (candidate : string) : SymlinkTarget =
        match parse candidate with
        | Ok target -> target
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// Re-check the invariant of a value that may not have come from `parse`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null payload would otherwise
    /// be stored as a symlink target that `checkInvariants` calls sound and
    /// that crashes only later, when some unrelated resolution happens to
    /// traverse it.
    let assertValid (context : string) (target : SymlinkTarget) : SymlinkTarget =
        match target with
        | SymlinkTarget raw ->

        match parse raw with
        | Ok _ -> target
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SymlinkTarget that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SymlinkTarget.parse instead."

    /// The path structure of the target, for a resolution walk to splice in.
    /// Total: `parse` has already discharged every rule `UnixPath.parse`
    /// enforces.
    let toUnixPath (target : SymlinkTarget) : UnixPath =
        let raw = toString target

        match UnixPath.parse raw with
        | Ok path -> path
        | Error error ->
            failwith
                $"SymlinkTarget.toUnixPath: %s{UnixPath.describe error} (got %s{raw}). Every SymlinkTarget satisfies UnixPath's invariant, so this cannot have come from SymlinkTarget.parse."

    /// The bytes `readlink(2)` hands back, and whose length is the link's
    /// `st_size`. Without a terminator: `readlink` does not write one.
    let toUtf8 (target : SymlinkTarget) : ImmutableArray<byte> =
        toString target |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange
