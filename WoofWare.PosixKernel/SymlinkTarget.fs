namespace WoofWare.PosixKernel

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
    | SymlinkTarget of target : UnixByteString

    /// The target rendered for a diagnostic; see `UnixByteString.toEscaped`.
    override this.ToString () : string =
        match this with
        | SymlinkTarget target -> UnixByteString.toEscaped target

/// <summary>
/// Why a candidate is not usable as the target of a symbolic link.
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
    /// <remarks>
    /// See <c>UnixPathTextDefect</c>. Only <c>parse</c>, which takes a .NET string, reports this.
    /// </remarks>
    | Text of defect : UnixPathTextDefect

[<RequireQualifiedAccess>]
module SymlinkTarget =
    /// <summary>
    /// The bytes <c>readlink(2)</c> hands back when asked about a symlink which points at this target.
    /// </summary>
    /// <remarks>
    /// The length of these bytes is the link's <c>st_size</c>.
    ///
    /// There is no NUL terminator, because <c>readlink</c> does not write one.
    /// </remarks>
    let toByteString (target : SymlinkTarget) : UnixByteString =
        match target with
        | SymlinkTarget target -> target

    /// The target as a .NET string, or `None` if its bytes are not valid UTF-8.
    let tryToString (target : SymlinkTarget) : string option =
        UnixByteString.tryToString (toByteString target)

    /// The target rendered for a diagnostic; see `UnixByteString.toEscaped`.
    let toEscaped (target : SymlinkTarget) : string =
        UnixByteString.toEscaped (toByteString target)

    /// Take a byte string as a symlink target, or explain why it is not one.
    ///
    /// Never reports `SymlinkTargetError.Text`: a `UnixByteString` has no text
    /// defects.
    let ofByteString (candidate : UnixByteString) : Result<SymlinkTarget, SymlinkTargetError> =
        if UnixByteString.length candidate = 0 then
            Error SymlinkTargetError.Empty
        else
            Ok (SymlinkTarget candidate)

    /// <summary>
    /// Parse a symlink target.
    /// </summary>
    /// <remarks>
    /// Never throws. The target is the UTF-8 encoding of <c>candidate</c>.
    /// </remarks>
    let parse (candidate : string) : Result<SymlinkTarget, SymlinkTargetError> =
        if System.String.IsNullOrEmpty candidate then
            Error SymlinkTargetError.Empty
        else

        match UnixByteString.ofString candidate with
        | Error defect -> Error (SymlinkTargetError.Text defect)
        | Ok bytes -> ofByteString bytes

    /// <summary>
    /// Human-readable description of this failure to represent a <c>SymlinkTarget</c>.
    /// </summary>
    let describe (error : SymlinkTargetError) : string =
        match error with
        | SymlinkTargetError.Empty ->
            "symlink target is null or empty; Linux rejects that with ENOENT while macOS accepts it, so this library declines to represent it"
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

        let raw = UnixByteString.assertValid context raw

        match ofByteString raw with
        | Ok _ -> target
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SymlinkTarget that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SymlinkTarget.parse instead."

    /// <summary>
    /// The path structure of the target, for a resolution walk to splice in.
    /// </summary>
    /// <remarks>
    /// Every target is a path, so this cannot fail.
    /// </remarks>
    let toUnixPath (target : SymlinkTarget) : UnixPath =
        UnixPath.ofByteString (toByteString target)
