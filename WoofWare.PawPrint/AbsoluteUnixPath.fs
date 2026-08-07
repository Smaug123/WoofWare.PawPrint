namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Text

/// Why a candidate string is not a path `getcwd(3)` could ever have returned.
///
/// A DU rather than a message string so that callers — most usefully the
/// property tests — can assert *which* rule a rejection tripped, rather than
/// pattern-matching on prose. `AbsoluteUnixPath.describe` renders these for
/// humans.
[<RequireQualifiedAccess>]
type AbsoluteUnixPathError =
    /// The candidate was null or empty. `getcwd` never yields an empty string:
    /// the shortest path it can return is the root, "/".
    | Empty
    /// The candidate did not begin with the directory separator, so it names a
    /// location relative to some other directory rather than an absolute one.
    /// Checked before every other rule, so a candidate that is *both*
    /// unrooted and otherwise malformed reports this.
    | NotRooted
    /// The candidate contained a NUL at this UTF-16 index. NUL terminates a C
    /// string, so such a path could not survive the trip through the
    /// `char*`-shaped `SystemNative_*` boundary intact.
    | ContainsNul of index : int
    /// The candidate contained an unpaired UTF-16 surrogate at this index, so
    /// it has no UTF-8 encoding at all. Real Unix paths are arbitrary
    /// non-NUL byte strings; PawPrint models the UTF-8-encodable subset, which
    /// is precisely the set CoreLib can round-trip back to a `string` through
    /// `Marshal.PtrToStringUTF8`.
    | UnpairedSurrogate of index : int
    /// Two consecutive separators, i.e. a zero-length segment beginning at this
    /// index. The kernel collapses these, so `getcwd` never reports one.
    | EmptySegment of index : int
    /// A separator at the end of a path other than the root. `getcwd` returns
    /// "/" for the root and an unterminated path for everything else.
    | TrailingSeparator
    /// A "." or ".." segment beginning at this index. `getcwd` returns a
    /// fully-resolved path, so neither can appear in its output.
    | UnresolvedSegment of segment : string * index : int

/// An absolute, fully-resolved Unix path: exactly the shape `getcwd(3)` can
/// return, and hence the only shape PawPrint's simulated current directory is
/// allowed to take.
///
/// The case is private, so the only way to *construct* one is
/// `AbsoluteUnixPath.parse` (or the `root` constant). That is the point:
/// consumers — the `SystemNative_GetCwd` handler, and whatever simulated
/// filesystem comes later — receive a proof that the path is rooted,
/// separator-normalised, resolved, and UTF-8 encodable, instead of a promise
/// they would each have to re-check.
///
/// One hole `private` cannot close: `Unchecked.defaultof<AbsoluteUnixPath>`
/// (and C#'s `default(AbsoluteUnixPath)`) bypasses every constructor and yields
/// a value whose payload is null — this is a struct, but a reference-typed
/// single-case union would have exactly the same hole. Nothing in PawPrint
/// produces one, and no *interior* consumer should re-check; instead the
/// boundaries that accept a path from outside the library assert the invariant
/// once, via `AbsoluteUnixPath.assertValid`. That is the gospel's "when types
/// can't express an invariant, assert it".
[<Struct>]
type AbsoluteUnixPath =
    private
    | AbsoluteUnixPath of path : string

    override this.ToString () : string =
        match this with
        | AbsoluteUnixPath path -> path

[<RequireQualifiedAccess>]
module AbsoluteUnixPath =
    /// The Unix directory separator. There is only one — unlike Windows,
    /// Unix has no alternate separator to normalise away.
    [<Literal>]
    let separator : char = '/'

    /// The root directory, "/". The one absolute path that is legally
    /// separator-terminated, and the only one guaranteed to exist on any Unix.
    let root : AbsoluteUnixPath = AbsoluteUnixPath "/"

    let toString (path : AbsoluteUnixPath) : string =
        match path with
        | AbsoluteUnixPath path -> path

    /// First per-character defect in `candidate`, scanning left to right, or
    /// `None` if there is none. Separate from the segment rules because it has
    /// to consider surrogate *pairs*, so it cannot be a per-character predicate.
    let private firstCharacterDefect (candidate : string) : AbsoluteUnixPathError option =
        let mutable i = 0
        let mutable result = None

        while result.IsNone && i < candidate.Length do
            let c = candidate.[i]

            if c = '\000' then
                result <- Some (AbsoluteUnixPathError.ContainsNul i)
            elif Char.IsHighSurrogate c then
                if i + 1 < candidate.Length && Char.IsLowSurrogate candidate.[i + 1] then
                    // A well-formed pair; step over its low half too, so that
                    // the low surrogate is not itself reported as unpaired.
                    i <- i + 1
                else
                    result <- Some (AbsoluteUnixPathError.UnpairedSurrogate i)
            elif Char.IsLowSurrogate c then
                // A low surrogate not consumed by the branch above has no high
                // half preceding it.
                result <- Some (AbsoluteUnixPathError.UnpairedSurrogate i)

            i <- i + 1

        result

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

    /// Parse a host-supplied string into an absolute Unix path, or explain why
    /// it is not one. Total: never throws, for any input including null.
    ///
    /// This deliberately *rejects* rather than normalises. A caller who passes
    /// "/a/../b" or "/a/" has a path the kernel would never have produced, and
    /// silently rewriting it would both hide the caller's bug and — for ".." —
    /// perform a resolution that is only correct in the absence of symlinks.
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

    /// Human-readable rendering of a rejection, for the `failwith` at whichever
    /// configuration boundary did the parsing.
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

    /// Re-check the invariant of a value that may not have come from `parse`.
    /// Returns it unchanged if it is sound, and fails loudly naming `context`
    /// if it is not.
    ///
    /// Only for boundaries that accept an `AbsoluteUnixPath` from outside the
    /// library — today, `EmulatedKernel.withCurrentDirectory`. The only value
    /// this can reject is `Unchecked.defaultof` / C# `default` (see the type's
    /// doc comment); catching it here turns a null-reference failure deep in
    /// the first `SystemNative_GetCwd` into a configuration-time error that
    /// says which knob was wrong. Interior consumers must *not* call this:
    /// re-validating a proof everywhere is precisely what the type exists to
    /// avoid.
    let assertValid (context : string) (path : AbsoluteUnixPath) : AbsoluteUnixPath =
        match path with
        | AbsoluteUnixPath raw ->

        match parse raw with
        | Ok _ -> path
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. An AbsoluteUnixPath that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with AbsoluteUnixPath.parse instead."

    /// Parse, or fail loudly naming the configuration knob at fault. For
    /// boundaries whose caller has no way to recover from a bad path — a
    /// mis-set `KernelConfig` field is a host bug, not a runtime condition.
    let parseOrFail (context : string) (candidate : string) : AbsoluteUnixPath =
        match parse candidate with
        | Ok path -> path
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// Strict UTF-8 encoder. `parse` has already rejected the only inputs that
    /// could trigger a fallback (unpaired surrogates), so the throwing
    /// configuration is an assertion that the type's invariant holds rather
    /// than a reachable error path — a silent U+FFFD substitution here would
    /// hand the guest bytes that do not decode back to the configured path.
    let private utf8 : UTF8Encoding = UTF8Encoding (false, true)

    /// The path as the NUL-free byte string a Unix kernel would hand back.
    /// Note this is the encoding *without* a terminator; callers that need a C
    /// string append the NUL themselves, because whether there is room for it
    /// is exactly what `getcwd`'s ERANGE check is about.
    let toUtf8 (path : AbsoluteUnixPath) : ImmutableArray<byte> =
        toString path |> utf8.GetBytes |> ImmutableArray.CreateRange
