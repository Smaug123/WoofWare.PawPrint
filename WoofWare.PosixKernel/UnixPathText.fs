namespace WoofWare.PosixKernel

open System
open System.Text

/// <summary>
/// A defect that makes a candidate .NET string unusable as <i>any</i> Unix path,
/// absolute or relative, whole path or single component.
/// </summary>
///
/// <remarks>
/// This doesn't cover path <i>syntax</i> rules such as rootedness, empty segments,
/// or unresolved "." and ".." segments.
/// It's only about whether the string itself is well-formed.
/// </remarks>
[<RequireQualifiedAccess>]
type UnixPathTextDefect =
    /// <summary>
    /// A NUL at this UTF-16 index.
    /// </summary>
    | ContainsNul of index : int
    /// <summary>An unpaired UTF-16 surrogate at this UTF-16 index.</summary>
    /// <remarks>
    /// This is a representable .NET string, but since it has no UTF-8 encoding, we do not attempt to
    /// convert it to a Unix path.
    /// </remarks>
    | UnpairedSurrogate of index : int

[<RequireQualifiedAccess>]
module UnixPathText =
    /// <summary>
    /// The Unix directory separator. (Unlike Windows, Unix has no alternate separator to normalise away.)
    /// </summary>
    [<Literal>]
    let separator : char = '/'

    /// <summary>
    /// Locate the first defect in <c>candidate</c> preventing the string from being a valid Unix path.
    /// </summary>
    ///
    /// <returns><c>None</c> if the string is legal.</returns>
    ///
    /// <remarks>
    /// Scans left-to-right.
    /// Treats the null string as being legal, because within-library callers already validate against null.
    /// </remarks>
    let firstDefect (candidate : string) : UnixPathTextDefect option =
        // Note: this function cannot be expressed as a per-character predicate, because
        // well-formed surrogate *pairs* are legal even thought their halves are not legal alone.
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

    /// <summary>
    /// Human-readable rendering of a defect.
    /// </summary>
    let describe (defect : UnixPathTextDefect) : string =
        match defect with
        | UnixPathTextDefect.ContainsNul index ->
            $"contains a NUL at index %d{index}, which cannot survive a C string boundary"
        | UnixPathTextDefect.UnpairedSurrogate index ->
            $"contains an unpaired UTF-16 surrogate at index %d{index}, so it has no UTF-8 encoding"

    /// <summary>
    /// Strict UTF-8 encoder which throws on invalid bytes.
    /// </summary>
    ///
    /// <remarks>
    /// Intended to be used after validation with <c>firstDefect</c>, which already rejects the only inputs that
    /// could cause the encoder to throw.
    /// </remarks>
    let utf8 : UTF8Encoding = UTF8Encoding (false, true)
