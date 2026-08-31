namespace WoofWare.PosixKernel

open System
open System.Collections.Immutable

/// <summary>
/// Why a candidate .NET string is not a path <c>getcwd(3)</c> could ever have returned.
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
    /// </remarks>
    | ContainsNul of index : int
    /// <summary>
    /// The candidate contained an unpaired UTF-16 surrogate at this UTF-16 index, so
    /// it has no UTF-8 encoding at all.
    /// </summary>
    /// <remarks>
    /// Real Unix paths are arbitrary non-NUL byte strings; we only model the
    /// UTF-8-encodable subset.
    /// </remarks>
    | UnpairedSurrogate of index : int
    /// <summary>
    /// Two consecutive separators, i.e. there is a zero-length segment beginning at this character index.
    /// </summary>
    /// <remarks>The kernel collapses these, so <c>getcwd</c> never reports one.</remarks>
    | EmptySegment of index : int
    /// <summary>There was a separator at the end of a path, and the path was not literally the root.</summary>
    /// <remarks><c>getcwd</c> returns "/" for the root and an unterminated path for everything else.</remarks>
    | TrailingSeparator
    /// <summary>The path contained a "." or ".." segment beginning at this character index.</summary>
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
    | AbsoluteUnixPath of path : string

    /// <summary>
    /// Round-trippable string representation.
    /// </summary>
    override this.ToString () : string =
        match this with
        | AbsoluteUnixPath path -> path

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
    let root : AbsoluteUnixPath = AbsoluteUnixPath "/"

    /// <summary>
    /// Round-trippable string representation.
    /// </summary>
    let toString (path : AbsoluteUnixPath) : string =
        match path with
        | AbsoluteUnixPath path -> path

    /// <summary>
    /// First encoding defect in <c>candidate</c>, scanning left to right, or <c>None</c>
    /// if there is none.
    /// </summary>
    let private firstCharacterDefect (candidate : string) : AbsoluteUnixPathError option =
        // Shared with `UnixPath` via `UnixPathText`, so that the two path shapes
        // cannot drift on which strings survive the `char*` boundary; only the
        // mapping into this type's error DU is local.
        UnixPathText.firstDefect candidate
        |> Option.map (fun defect ->
            match defect with
            | UnixPathTextDefect.ContainsNul index -> AbsoluteUnixPathError.ContainsNul index
            | UnixPathTextDefect.UnpairedSurrogate index -> AbsoluteUnixPathError.UnpairedSurrogate index
        )

    /// First defect in `candidate`'s segment structure, or `None` if there is
    /// none. `candidate` must already be known non-empty and separator-rooted.
    let private firstSegmentDefect (candidate : string) : AbsoluteUnixPathError option =
        if candidate = "/" then
            // The root is the one path whose sole separator is also its last
            // character; every rule below would otherwise reject it.
            None
        elif candidate.[candidate.Length - 1] = separator then
            Some AbsoluteUnixPathError.TrailingSeparator
        else

        let segments = candidate.Substring(1).Split separator

        // Index within `candidate` at which each segment starts: past the
        // leading separator, then past every earlier segment and its separator.
        // `Array.scan` yields one more element than `segments`, so drop the
        // trailing running total.
        let offsets =
            segments |> Array.scan (fun offset (seg : string) -> offset + seg.Length + 1) 1

        Array.zip segments offsets.[.. segments.Length - 1]
        |> Array.tryPick (fun (segment, offset) ->
            if segment.Length = 0 then
                Some (AbsoluteUnixPathError.EmptySegment offset)
            elif segment = "." || segment = ".." then
                Some (AbsoluteUnixPathError.UnresolvedSegment (segment, offset))
            else
                None
        )

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

        match firstCharacterDefect candidate with
        | Some defect -> Error defect
        | None ->

        match firstSegmentDefect candidate with
        | Some defect -> Error defect
        | None -> Ok (AbsoluteUnixPath candidate)

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
            $"path contains an empty segment (a repeated '%c{separator}') at index %d{index}"
        | AbsoluteUnixPathError.TrailingSeparator ->
            $"path ends with '%c{separator}'; only the root \"/\" may be separator-terminated"
        | AbsoluteUnixPathError.UnresolvedSegment (segment, index) ->
            $"path contains an unresolved \"%s{segment}\" segment at index %d{index}; getcwd returns fully-resolved paths"

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
        // whose payload is null; the `private` on the union case, plus the restrictions
        // enforced by the `parse` method, stops every other route.
        match path with
        | AbsoluteUnixPath raw ->

        match parse raw with
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

    /// <summary>Return the path as exactly the NUL-free byte string the Unix kernel would use.</summary>
    /// <remarks>
    /// This is the encoding <i>without</i> a null terminator; callers that need a C
    /// string must append the NUL themselves.
    /// </remarks>
    let toUtf8 (path : AbsoluteUnixPath) : ImmutableArray<byte> =
        toString path |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange
