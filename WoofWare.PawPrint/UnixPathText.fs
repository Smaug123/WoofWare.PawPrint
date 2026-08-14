namespace WoofWare.PawPrint

open System
open System.Text

/// A defect that makes a candidate string unusable as *any* Unix path,
/// absolute or relative, whole path or single component. These are the two
/// rules that come from the boundary rather than from path syntax: they hold
/// wherever a `string` has to survive the trip through the `char*`-shaped
/// `SystemNative_*` interface.
///
/// Path *syntax* rules — rootedness, empty segments, unresolved "." and ".."
/// segments — are not here, because they differ between the shapes PawPrint
/// models: `AbsoluteUnixPath` rejects all of them, while a guest-supplied
/// `UnixPath` accepts all of them.
[<RequireQualifiedAccess>]
type UnixPathTextDefect =
    /// A NUL at this UTF-16 index. NUL terminates a C string, so a path
    /// containing one could not survive the boundary intact: the kernel would
    /// see a shorter path than the guest named.
    | ContainsNul of index : int
    /// An unpaired UTF-16 surrogate at this index, so the string has no UTF-8
    /// encoding at all. Real Unix paths are arbitrary non-NUL byte strings;
    /// PawPrint models the UTF-8-encodable subset, which is precisely the set
    /// CoreLib can round-trip back to a `string` through
    /// `Marshal.PtrToStringUTF8`.
    | UnpairedSurrogate of index : int

[<RequireQualifiedAccess>]
module UnixPathText =
    /// The Unix directory separator. There is only one — unlike Windows, Unix
    /// has no alternate separator to normalise away.
    [<Literal>]
    let separator : char = '/'

    /// First defect in `candidate`, scanning left to right, or `None` if there
    /// is none. Total: never throws, and treats `null` as defect-free (it has
    /// no characters to object to; callers reject it on their own grounds).
    ///
    /// Cannot be expressed as a per-character predicate, because well-formed
    /// surrogate *pairs* are legal and their halves are not legal alone.
    let firstDefect (candidate : string) : UnixPathTextDefect option =
        if isNull candidate then
            None
        else

        let mutable i = 0
        let mutable result = None

        while result.IsNone && i < candidate.Length do
            let c = candidate.[i]

            if c = '\000' then
                result <- Some (UnixPathTextDefect.ContainsNul i)
            elif Char.IsHighSurrogate c then
                if i + 1 < candidate.Length && Char.IsLowSurrogate candidate.[i + 1] then
                    // A well-formed pair; step over its low half too, so that
                    // the low surrogate is not itself reported as unpaired.
                    i <- i + 1
                else
                    result <- Some (UnixPathTextDefect.UnpairedSurrogate i)
            elif Char.IsLowSurrogate c then
                // A low surrogate not consumed by the branch above has no high
                // half preceding it.
                result <- Some (UnixPathTextDefect.UnpairedSurrogate i)

            i <- i + 1

        result

    /// Human-readable rendering of a defect, for the diagnostics of whichever
    /// path type did the parsing.
    let describe (defect : UnixPathTextDefect) : string =
        match defect with
        | UnixPathTextDefect.ContainsNul index ->
            $"contains a NUL at index %d{index}, which cannot survive a C string boundary"
        | UnixPathTextDefect.UnpairedSurrogate index ->
            $"contains an unpaired UTF-16 surrogate at index %d{index}, so it has no UTF-8 encoding"

    /// Strict UTF-8 encoder. `firstDefect` has already rejected the only inputs
    /// that could trigger a fallback (unpaired surrogates), so the throwing
    /// configuration is an assertion that the caller's invariant holds rather
    /// than a reachable error path — a silent U+FFFD substitution here would
    /// hand the guest bytes that do not decode back to the path it named.
    let utf8 : UTF8Encoding = UTF8Encoding (false, true)
