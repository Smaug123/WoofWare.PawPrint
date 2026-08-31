namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// <summary>
/// The target of a symbolic link.
/// </summary>
/// <remarks>
/// See also <c>UnixPath</c>, which represents the paths a guest can construct.
/// Every <c>SymlinkTarget</c> is a valid <c>UnixPath</c>.
/// </remarks>
[<Struct>]
type SymlinkTarget =
    private
    /// <summary>
    /// Verbatim rather than parsed.
    /// </summary>
    /// <remarks>
    /// <c>readlink(2)</c> returns the stored bytes unchanged, and <c>lstat</c> reports their length as the link's
    /// <c>st_size</c>, so a link created with target "a//b/" must read back as "a//b/" — a difference a
    /// guest really can see.
    /// </remarks>
    | SymlinkTarget of target : string

    /// <summary>
    /// The exact string that was used to construct this target.
    /// </summary>
    override this.ToString () : string =
        match this with
        | SymlinkTarget target -> target

/// <summary>
/// Why a string is not usable as the target of a symbolic link.
/// </summary>
[<RequireQualifiedAccess>]
type SymlinkTargetError =
    /// <summary>
    /// The candidate was null or empty.
    /// </summary>
    /// <remarks>
    /// <c>symlink(2)</c> on Linux rejects an empty target with <c>ENOENT</c>.
    /// Darwin instead accepts it, creating a link that then fails to resolve.
    ///
    /// WoofWare.PosixKernel doesn't parameterise over those options, but simply
    /// refuses to represent the situation at all.
    /// </remarks>
    | Empty
    /// <summary>
    /// The candidate could not survive the <c>char*</c> boundary.
    /// </summary>
    /// <remarks>See <c>UnixPathTextDefect</c>.</remarks>
    | Text of defect : UnixPathTextDefect

[<RequireQualifiedAccess>]
module SymlinkTarget =
    /// <summary>
    /// The exact string that was used to construct this target.
    /// </summary>
    let toString (target : SymlinkTarget) : string =
        match target with
        | SymlinkTarget target -> target

    /// <summary>
    /// Parse a symlink target.
    /// </summary>
    /// <remarks>
    /// Never throws.
    /// </remarks>
    let parse (candidate : string) : Result<SymlinkTarget, SymlinkTargetError> =
        if System.String.IsNullOrEmpty candidate then
            Error SymlinkTargetError.Empty
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (SymlinkTargetError.Text defect)
        | None -> Ok (SymlinkTarget candidate)

    /// <summary>
    /// Human-readable description of this failure to represent a <c>SymlinkTarget</c>.
    /// </summary>
    let describe (error : SymlinkTargetError) : string =
        match error with
        | SymlinkTargetError.Empty ->
            "symlink target is null or empty; Linux rejects that with ENOENT while macOS accepts it, so PawPrint declines to represent it"
        | SymlinkTargetError.Text defect -> $"symlink target %s{UnixPathText.describe defect}"

    /// <summary>
    /// Parse a symlink target, throwing if the parse failed.
    /// </summary>
    /// <remarks>
    /// This is <c>SymlinkTarget.parse</c> except it throws instead of returning an error <c>Result</c>.
    /// </remarks>
    let parseOrFail (context : string) (candidate : string) : SymlinkTarget =
        match parse candidate with
        | Ok target -> target
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// <summary>
    /// Re-check the invariant of a value.
    /// </summary>
    /// <remarks>
    /// You don't need to call this for a target which came from <c>SymlinkTarget.parse</c>.
    /// (Use it e.g. when the input might have been <c>Unchecked.defaultof</c>.)
    /// </remarks>
    let assertValid (context : string) (target : SymlinkTarget) : SymlinkTarget =
        match target with
        | SymlinkTarget raw ->

        match parse raw with
        | Ok _ -> target
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SymlinkTarget that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SymlinkTarget.parse instead."

    /// <summary>
    /// The path structure of the target, for a resolution walk to splice in.
    /// </summary>
    /// <remarks>
    /// Throws only if the input would fail <c>SymlinkTarget.assertValid</c>.
    /// Cannot throw on an input which came from <c>SymlinkTarget.parse</c>.
    /// </remarks>
    let toUnixPath (target : SymlinkTarget) : UnixPath =
        let raw = toString target

        match UnixPath.parse raw with
        | Ok path -> path
        | Error error ->
            // `parse` has already discharged every rule `UnixPath.parse`
            // enforces. This case should never happen, unless the user supplied `null` as the input.
            failwith
                $"SymlinkTarget.toUnixPath: %s{UnixPath.describe error} (got %s{raw}). Every SymlinkTarget satisfies UnixPath's invariant, so this cannot have come from SymlinkTarget.parse."

    /// <summary>
    /// The bytes <c>readlink(2)</c> hands back when asked about a symlink which points at this target.
    /// </summary>
    /// <remarks>
    /// The resulting array's length is the link's <c>st_size</c>.
    ///
    /// There is no NUL terminator, because <c>readlink</c> does not write one.
    /// </remarks>
    let toUtf8 (target : SymlinkTarget) : ImmutableArray<byte> =
        toString target |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange
