namespace WoofWare.PosixKernel

open System

/// <summary>
/// Why a candidate is not a path <c>getcwd(3)</c> could ever have returned.
/// </summary>
[<RequireQualifiedAccess>]
type AbsoluteUnixPathError =
    /// <summary>
    /// The candidate was null or empty.
    /// </summary>
    /// <remarks>
    /// <c>getcwd</c> never yields an empty string: the shortest path it can return is the root, "/".
    /// </remarks>
    | Empty
    /// <summary>The candidate did not begin with the directory separator.</summary>
    /// <remarks>
    /// Such a string names a location relative to some other directory, rather than an absolute one.
    ///
    /// WoofWare.PosixKernel generally checks this before every other rule, so if a candidate is
    /// both unrooted <i>and</i> otherwise malformed, you get "not rooted".
    /// </remarks>
    | NotRooted
    /// <summary> The candidate contained a NUL byte at this UTF-16 character index.</summary>
    /// <remarks>
    /// NUL terminates a C string and cannot be part of a path.
    ///
    /// Only <c>parse</c>, which takes a .NET string, reports this.
    /// </remarks>
    | ContainsNul of index : int
    /// <summary>
    /// The candidate contained an unpaired UTF-16 surrogate at this UTF-16 index, so
    /// it has no UTF-8 encoding at all.
    /// </summary>
    /// <remarks>
    /// Only <c>parse</c>, which takes a .NET string, reports this.
    /// </remarks>
    | UnpairedSurrogate of index : int
    /// <summary>
    /// Two consecutive separators, i.e. there is a zero-length segment beginning at this byte index.
    /// </summary>
    /// <remarks>The kernel collapses these, so <c>getcwd</c> never reports one.</remarks>
    | EmptySegment of index : int
    /// <summary>There was a separator at the end of a path, and the path was not literally the root.</summary>
    /// <remarks><c>getcwd</c> returns "/" for the root and an unterminated path for everything else.</remarks>
    | TrailingSeparator
    /// <summary>The path contained a "." or ".." segment beginning at this byte index.</summary>
    /// <remarks><c>getcwd</c> returns a fully-resolved path, so neither can appear in its output.</remarks>
    | UnresolvedSegment of segment : string * index : int

/// <summary>
/// An absolute, fully-resolved Unix path: exactly the shape <c>getcwd(3)</c> can
/// return, and hence the only shape the simulated current directory is allowed to take.
/// </summary>
///
/// <remarks>
/// Construct via <c>AbsoluteUnixPath.parse</c> (or the <c>AbsoluteUnixPath.root</c> constant).
/// </remarks>
[<Struct>]
type AbsoluteUnixPath =
    private
    | AbsoluteUnixPath of path : UnixByteString

    /// The path rendered for a diagnostic; see `UnixByteString.toEscaped`.
    override this.ToString () : string =
        match this with
        | AbsoluteUnixPath path -> UnixByteString.toEscaped path

[<RequireQualifiedAccess>]
module AbsoluteUnixPath =
    /// <summary>
    /// The Unix directory separator.
    /// </summary>
    [<Literal>]
    let separator : char = UnixPathText.separator

    /// <summary>The root directory, "/".</summary>
    /// <remarks>
    /// The one absolute path that is legally separator-terminated, and the only one guaranteed to exist on any Unix.
    /// </remarks>
    let root : AbsoluteUnixPath =
        match UnixByteString.ofString "/" with
        | Ok bytes -> AbsoluteUnixPath bytes
        | Error defect -> failwith $"AbsoluteUnixPath.root: %s{UnixPathText.describe defect}"

    /// The path's bytes, exactly as `getcwd(3)` would hand them back.
    let toByteString (path : AbsoluteUnixPath) : UnixByteString =
        match path with
        | AbsoluteUnixPath path -> path

    /// The path as a .NET string, or `None` if its bytes are not valid UTF-8.
    let tryToString (path : AbsoluteUnixPath) : string option =
        UnixByteString.tryToString (toByteString path)

    /// The path rendered for a diagnostic; see `UnixByteString.toEscaped`.
    let toEscaped (path : AbsoluteUnixPath) : string =
        UnixByteString.toEscaped (toByteString path)

    /// First defect in `candidate`'s segment structure, or `None` if there is
    /// none. `candidate` must already be known non-empty and separator-rooted.
    let private firstSegmentDefect (candidate : UnixByteString) : AbsoluteUnixPathError option =
        let bytes = UnixByteString.toBytes candidate

        if bytes.Length = 1 then
            // The root is the one path whose sole separator is also its last
            // byte; every rule below would otherwise reject it.
            None
        elif bytes.[bytes.Length - 1] = UnixPathText.separatorByte then
            Some AbsoluteUnixPathError.TrailingSeparator
        else

        // Each segment runs from just past one separator to the next, starting
        // past the leading one.
        let rec check (start : int) : AbsoluteUnixPathError option =
            if start > bytes.Length then
                None
            else

            let next = bytes.IndexOf (UnixPathText.separatorByte, start)
            let finish = if next < 0 then bytes.Length else next

            let result =
                match finish - start with
                | 0 -> Some (AbsoluteUnixPathError.EmptySegment start)
                | 1 when bytes.[start] = 46uy -> Some (AbsoluteUnixPathError.UnresolvedSegment (".", start))
                | 2 when bytes.[start] = 46uy && bytes.[start + 1] = 46uy ->
                    Some (AbsoluteUnixPathError.UnresolvedSegment ("..", start))
                | _ -> None

            match result with
            | Some _ -> result
            | None -> check (finish + 1)

        check 1

    /// Take a byte string as an absolute path, or explain why it is not one.
    ///
    /// Never reports `ContainsNul` or `UnpairedSurrogate`: a `UnixByteString`
    /// can contain neither.
    let ofByteString (candidate : UnixByteString) : Result<AbsoluteUnixPath, AbsoluteUnixPathError> =
        let candidate = UnixByteString.assertValid "AbsoluteUnixPath.ofByteString" candidate
        let bytes = UnixByteString.toBytes candidate

        if bytes.Length = 0 then
            Error AbsoluteUnixPathError.Empty
        elif bytes.[0] <> UnixPathText.separatorByte then
            Error AbsoluteUnixPathError.NotRooted
        else

        match firstSegmentDefect candidate with
        | Some defect -> Error defect
        | None -> Ok (AbsoluteUnixPath candidate)

    /// <summary>
    /// Parse a host-supplied string into an absolute Unix path, or explain why
    /// it is not one.
    /// </summary>
    ///
    /// <remarks>
    /// This rejects invalid input (such as components which are just "..") rather than performing any normalisation.
    /// (Paths cannot be normalised correctly without knowing whether there are symlinks.)
    /// </remarks>
    let parse (candidate : string) : Result<AbsoluteUnixPath, AbsoluteUnixPathError> =
        if String.IsNullOrEmpty candidate then
            Error AbsoluteUnixPathError.Empty
        elif candidate.[0] <> separator then
            Error AbsoluteUnixPathError.NotRooted
        else

        match UnixByteString.ofString candidate with
        | Error (UnixPathTextDefect.ContainsNul index) -> Error (AbsoluteUnixPathError.ContainsNul index)
        | Error (UnixPathTextDefect.UnpairedSurrogate index) -> Error (AbsoluteUnixPathError.UnpairedSurrogate index)
        | Ok bytes -> ofByteString bytes

    /// <summary>
    /// Human-readable rendering of a rejection.
    /// </summary>
    let describe (error : AbsoluteUnixPathError) : string =
        match error with
        | AbsoluteUnixPathError.Empty -> "path is null or empty; the shortest absolute Unix path is \"/\""
        | AbsoluteUnixPathError.NotRooted -> $"path does not begin with '%c{separator}', so it is not absolute"
        | AbsoluteUnixPathError.ContainsNul index ->
            $"path contains a NUL at index %d{index}, which cannot survive a C string boundary"
        | AbsoluteUnixPathError.UnpairedSurrogate index ->
            $"path contains an unpaired UTF-16 surrogate at index %d{index}, so it has no UTF-8 encoding"
        | AbsoluteUnixPathError.EmptySegment index ->
            $"path contains an empty segment (a repeated '%c{separator}') at byte index %d{index}"
        | AbsoluteUnixPathError.TrailingSeparator ->
            $"path ends with '%c{separator}'; only the root \"/\" may be separator-terminated"
        | AbsoluteUnixPathError.UnresolvedSegment (segment, index) ->
            $"path contains an unresolved \"%s{segment}\" segment at byte index %d{index}; getcwd returns fully-resolved paths"

    /// <summary>
    /// Re-check the invariant of a value that may not have come from <c>parse</c>,
    /// throwing (naming <c>context</c> in the error) if it does not hold.
    /// </summary>
    ///
    /// <remarks>
    /// The <c>parse</c> constructor already ensures the invariant holds, so there's
    /// no need to call this if you know the input came from <c>parse</c>.
    /// </remarks>
    let assertValid (context : string) (path : AbsoluteUnixPath) : AbsoluteUnixPath =
        // The only value this can actually reject is `Unchecked.defaultof` / C# `default`,
        // whose payload is a forged `UnixByteString`; the `private` on the union case, plus
        // the restrictions enforced by the constructors, stops every other route.
        match path with
        | AbsoluteUnixPath raw ->

        let raw = UnixByteString.assertValid context raw

        match ofByteString raw with
        | Ok _ -> path
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. An AbsoluteUnixPath that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with AbsoluteUnixPath.parse instead."

    /// <summary>Parse a string as an absolute path, or throw if it's not valid.</summary>
    /// <remarks>
    /// This is <c>AbsoluteUnixPath.parse</c>, except it throws instead of returning an error description.
    /// </remarks>
    let parseOrFail (context : string) (candidate : string) : AbsoluteUnixPath =
        match parse candidate with
        | Ok path -> path
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"
