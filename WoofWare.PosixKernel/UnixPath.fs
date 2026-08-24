namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Why a .NET string is not usable as a single Unix directory-entry name.
[<RequireQualifiedAccess>]
type FileNameError =
    /// <summary>The candidate was null or empty.</summary>
    /// <remarks>
    /// No directory contains an entry with the empty name; a path that appears
    /// to ask for one ("a//b", "a/") has a zero-length <i>segment</i>, which
    /// <c>UnixPath.components</c> drops rather than turning into a name.
    /// </remarks>
    | Empty
    /// The candidate contained a separator at this index, so it names a path
    /// rather than a single entry within one directory.
    | ContainsSeparator of index : int
    /// <summary>The candidate could not survive the <c>char*</c> boundary.</summary>
    /// <remarks>See <c>UnixPathTextDefect</c>.</remarks>
    | Text of defect : UnixPathTextDefect
    /// <summary>
    /// The candidate was "." or "..".
    /// </summary>
    /// <remarks>
    /// Both are legal path <i>components</i>, and our <c>PathComponent</c> type
    /// represents them, but they aren't ever the name of an entry in a directory.
    /// WoofWare.PosixKernel derives those entries from the directory graph, rather than storing them.
    /// </remarks>
    | Reserved of name : string

/// <summary>
/// A single component of a Unix path that names an actual directory entry.
/// </summary>
/// <remarks>
/// This is non-empty, separator-free, NUL-free, UTF-8-encodable, and neither "." nor
/// "..".
///
/// Construct via <c>FileName.parse</c>.
/// </remarks>
[<Struct>]
type FileName =
    private
    | FileName of name : string

    /// Round-trippable string representation.
    override this.ToString () : string =
        match this with
        | FileName name -> name

[<RequireQualifiedAccess>]
module FileName =
    /// Round-trippable string representation.
    let toString (name : FileName) : string =
        match name with
        | FileName name -> name

    /// <summary>Parse a single directory-entry name, or explain why the candidate is not one.</summary>
    let parse (candidate : string) : Result<FileName, FileNameError> =
        if System.String.IsNullOrEmpty candidate then
            Error FileNameError.Empty
        else

        let separatorIndex = candidate.IndexOf UnixPathText.separator

        if separatorIndex >= 0 then
            Error (FileNameError.ContainsSeparator separatorIndex)
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (FileNameError.Text defect)
        | None ->

        if candidate = "." || candidate = ".." then
            Error (FileNameError.Reserved candidate)
        else
            Ok (FileName candidate)

    /// Human-readable rendering of a rejection.
    let describe (error : FileNameError) : string =
        match error with
        | FileNameError.Empty -> "name is null or empty, but no directory holds an entry with the empty name"
        | FileNameError.ContainsSeparator index ->
            $"name contains '%c{UnixPathText.separator}' at index %d{index}, so it is a path rather than a single entry name"
        | FileNameError.Text defect -> $"name %s{UnixPathText.describe defect}"
        | FileNameError.Reserved name ->
            $"\"%s{name}\" is a path component, not an entry name; PawPrint derives it from the directory graph rather than storing it"

    /// <summary>Parse a single directory-entry name, or throw.</summary>
    /// <remarks>This is <c>FileName.parse</c> except it throws.</remarks>
    let parseOrFail (context : string) (candidate : string) : FileName =
        match parse candidate with
        | Ok name -> name
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// Re-check the invariant of a value that may not have come from `parse`,
    /// failing loudly and naming `context` if it does not hold.
    ///
    /// Only for boundaries that accept a `FileName` from outside the library;
    /// interior consumers must not call it, because re-validating a proof
    /// everywhere is precisely what the type exists to avoid. Today's callers are
    /// the chokepoints through which a name enters or leaves the inode graph, so
    /// that a value which never went through `parse` is stopped before it becomes
    /// an entry no path could ever name.
    // The only value this can reject is `Unchecked.defaultof` / C# `default`,
    // whose payload is null: `private` on the union case stops every other route.
    // Without this, such a name reaches a directory's entry map and
    // `checkInvariants` calls the resulting graph sound.
    let assertValid (context : string) (name : FileName) : FileName =
        match name with
        | FileName raw ->

        match parse raw with
        | Ok _ -> name
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A FileName that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with FileName.parse instead."

    /// <summary>The name as the NUL-free byte string a Unix kernel would hand back from <c>readdir</c>.</summary>
    /// <remarks>Has no NUL terminator; callers that need a C string append the NUL themselves.</remarks>
    let toUtf8 (name : FileName) : ImmutableArray<byte> =
        toString name |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange

/// <summary>
/// One component of a guest-supplied path, between two separators.
/// </summary>
[<RequireQualifiedAccess>]
type PathComponent =
    /// <summary>".".</summary>
    /// <remarks>
    /// This is not a no-op, because it adds the constraint that the preceding segment must be a directory.
    /// That is, "a/." and "a" resolve differently when "a" is a regular file.
    /// </remarks>
    | Current
    /// <summary>"..".</summary>
    /// <remarks>
    /// Resolved against the <i>physical</i> parent recorded in the directory
    /// graph, not against the lexical predecessor in the path.
    /// This makes a difference when the walk crosses a symlink.
    /// </remarks>
    | Parent
    /// <summary>An ordinary entry name, to be looked up in the directory reached so far.</summary>
    | Name of name : FileName

/// <summary>
/// Why a .NET string is not usable as a guest-supplied Unix path.
/// </summary>
///
/// <remarks>
/// Far more permissive than <c>AbsoluteUnixPathError</c>, which instead describes what the <i>kernel can return</i>.
/// A guest may legitimately pass a relative path, repeated separators, a trailing separator, and "." or
/// ".." components.
/// </remarks>
[<RequireQualifiedAccess>]
type UnixPathError =
    /// <summary>The candidate was null.</summary>
    /// <remarks>
    /// Distinct from the <i>empty</i> path, whose parse is permitted.
    /// The empty string is a legal C string that the kernel only rejects at resolution time (with <c>ENOENT</c>).
    /// </remarks>
    | Null
    /// <summary>
    /// The candidate could not survive the <c>char*</c> boundary.
    /// </summary>
    /// <remarks>See <c>UnixPathTextDefect</c>.</remarks>
    | Text of defect : UnixPathTextDefect

/// <summary>
/// A path exactly as a guest handed it to a syscall.
/// </summary>
/// <remarks>
/// Possibly relative, possibly containing "." and ".." components, possibly with a trailing
/// separator.
///
/// This is the input to the <c>VirtualFileSystem</c> resolution walk.
///
/// Contrast <c>AbsoluteUnixPath</c>, which is the strictly narrower shape
/// <c>getcwd(3)</c> can <i>return</i>.
/// Every <c>AbsoluteUnixPath</c> is a <c>UnixPath</c> (see <c>UnixPath.ofAbsolute</c>); the
/// converse holds only for paths a resolution walk has already reduced.
///
/// Construct via <c>UnixPath.parse</c>.
///
/// Recover the path's structure on demand with <c>UnixPath.components</c> and <c>PathCursor</c>.
/// </remarks>
type UnixPath =
    private
        {
            /// <summary>
            /// The path exactly as it was handed to the syscall: separator runs
            /// and trailing separators intact, "." and ".." uninterpreted.
            /// </summary>
            ///
            /// <remarks>
            /// The only rules governing this are those enforced by <c>parse</c>.
            ///
            /// Darwin's length rules count the bytes in the path's byte buffer, so "a//b" and "a/b" are
            /// behaviourally distinct on Darwin. So we really do have to store it raw.
            /// </remarks>
            Raw : string
        }

/// <summary>
/// A position part-way through resolving a path, specifying a component or separator within the path.
/// </summary>
/// <remarks>
/// This is the shape a Unix kernel's own resolution state has, namely a
/// pathname buffer plus a pointer into it (XNU's `cn_pnbuf` and `ni_next`,
/// Linux's `nameidata`).
/// </remarks>
[<Struct>]
type PathCursor =
    private
        {
            /// <summary>
            /// The path text currently being resolved. Not necessarily the text
            /// the guest passed, because a symlink expansion may have replaced the original.
            /// </summary>
            ///
            /// <remarks>
            /// Always satisfies <c>UnixPath</c>'s invariant, because every way to
            /// build one takes a <c>UnixPath</c>.
            /// </remarks>
            Buffer : string
            /// <summary>
            /// How far through <c>Buffer</c> the walk has got, measured in chars of the UTF-16 string.
            /// </summary>
            /// <remarks>
            /// Always at a separator or at the end, never inside a component.
            /// </remarks>
            Offset : int
        }

[<RequireQualifiedAccess>]
module PathCursor =
    /// Begin walking a path, at its first component.
    let ofPath (path : UnixPath) : PathCursor =
        {
            Buffer = path.Raw
            Offset = 0
        }

    /// <summary>The text this cursor is walking.</summary>
    /// <remarks>Throws if we are walking the null string.</remarks>
    let private bufferOf (cursor : PathCursor) : string =
        match cursor.Buffer with
        | null ->
            failwith
                "PathCursor: this cursor's buffer is null, which it can only be if the cursor came from `Unchecked.defaultof` or C# `default`; obtain one from PathCursor.ofPath instead."
        | buffer -> buffer

    /// The buffer from the cursor onwards, separator runs intact.
    let private remainder (cursor : PathCursor) : string =
        (bufferOf cursor).Substring cursor.Offset

    /// Where the next component starts, or the end of the buffer if only
    /// separators remain.
    let private afterSeparators (cursor : PathCursor) : int =
        let buffer = bufferOf cursor
        let mutable index = cursor.Offset

        while index < buffer.Length && buffer.[index] = UnixPathText.separator do
            index <- index + 1

        index

    /// <summary>True when no component remains to be looked up.</summary>
    /// <remarks>A buffer holding only separators is exhausted: "a///" names only one component, not four.</remarks>
    let isExhausted (cursor : PathCursor) : bool =
        afterSeparators cursor = (bufferOf cursor).Length

    /// <summary>The next component, and the cursor positioned after it.</summary>
    /// <returns><c>None</c> when the path is exhausted; otherwise, the component that was at the cursor, and a new cursor which is advanced once.</returns>
    /// <remarks>Throws if the input cursor is into the null string.</remarks>
    let next (cursor : PathCursor) : (PathComponent * PathCursor) option =
        let buffer = bufferOf cursor
        let start = afterSeparators cursor

        if start = buffer.Length then
            None
        else

        let mutable finish = start

        while finish < buffer.Length && buffer.[finish] <> UnixPathText.separator do
            finish <- finish + 1

        let segment = buffer.Substring (start, finish - start)

        let component_ =
            match segment with
            | "." -> PathComponent.Current
            | ".." -> PathComponent.Parent
            | _ ->
                // Every rule `FileName.parse` enforces has already been
                // discharged: the segment is non-empty (the scan above stopped
                // at a non-separator), holds no separator (the scan stopped at
                // one), carries no text defect (`UnixPath.parse` scanned the
                // whole buffer), and is neither "." nor ".." (matched above).
                match FileName.parse segment with
                | Ok name -> PathComponent.Name name
                | Error error ->
                    failwith
                        $"PathCursor.next: segment \"%s{segment}\" of \"%s{buffer}\" was rejected as an entry name (%s{FileName.describe error}), but every value that can reach this has already been through UnixPath.parse, which excludes every reason a segment could be rejected"

        // Transcribed from XNU's `lookup`, which advances past the separator run
        // following a component "while the next character is a separator or the
        // end". The effect is that a run is collapsed to exactly one separator,
        // and a run with nothing after it is consumed entirely — so "a//b"
        // leaves "/b" but "a//" leaves nothing.
        //
        // Expanding a symbolic link copies from exactly
        // here, so these bytes are absent from the resulting buffer, and Darwin
        // compares that buffer's length against PATH_MAX. Measured on Darwin
        // 25.6.0: an "//a" remainder costs the same as "/a", while an "/a//b"
        // remainder costs one byte more than "/a/b".
        let mutable niNext = finish

        while niNext < buffer.Length
              && buffer.[niNext] = UnixPathText.separator
              && (niNext + 1 = buffer.Length || buffer.[niNext + 1] = UnixPathText.separator) do
            niNext <- niNext + 1

        Some (
            component_,
            { cursor with
                Offset = niNext
            }
        )

    /// <summary>How many bytes of pathname the walk still has in front of it, <i>not</i>
    /// counting the NUL a kernel keeps at the end of its buffer.
    /// </summary>
    /// <remarks>
    /// XNU's <c>ni_pathlen</c> is this plus one.
    ///
    /// We don't expose a count of remaining <i>characters</i>, because a kernel's
    /// pathname buffer is bytes and its limits are byte counts.
    /// </remarks>
    let remainingBytes (cursor : PathCursor) : int =
        UnixPathText.utf8.GetByteCount (remainder cursor)

    /// <summary>
    /// Expand a symbolic link: the target, followed by whatever the walk had
    /// left to resolve.
    /// </summary>
    /// <remarks>
    /// Call <c>SymlinkTarget.toUnixPath</c> to construct the input.
    ///
    /// The cursor must be one that's advanced at least once (we throw if it wasn't).
    /// Expanding a link replaces the component just consumed, so a cursor that has
    /// consumed nothing has nothing to replace.
    /// </remarks>
    let splice (target : UnixPath) (cursor : PathCursor) : PathCursor =
        // Validity first: a forged default has offset zero too, and would
        // otherwise be reported as the wrong mistake.
        let buffer = bufferOf cursor

        // `next` never returns offset zero — a component is at least one
        // character — so offset zero is exactly the condition to check.
        if cursor.Offset = 0 then
            failwith
                $"PathCursor.splice: the cursor into \"%s{buffer}\" has not consumed a component, so there is no symbolic link here to expand. Splice onto the cursor `next` returned, not one straight from `ofPath`."

        {
            Buffer = target.Raw + remainder cursor
            Offset = 0
        }

[<RequireQualifiedAccess>]
module UnixPath =
    /// <summary>Render back to the string a guest would have passed.</summary>
    /// <remarks>
    /// Exact: this returns the very text <c>parse</c> accepted, separator runs and all, so
    /// <c>toString (parse s) = s</c> for every <c>s</c> that parses.
    /// </remarks>
    let toString (path : UnixPath) : string = path.Raw

    /// <summary>True when the path began with a separator.</summary>
    /// <remarks>That is, path resolution for this path starts at the
    /// filesystem root rather than at a caller-supplied directory.
    /// </remarks>
    let isRooted (path : UnixPath) : bool =
        path.Raw.Length > 0 && path.Raw.[0] = UnixPathText.separator

    /// <summary>
    /// The path's components in order.
    /// </summary>
    /// <returns>No zero-length segments, but "." and
    /// ".." are preserved as <c>PathComponent.Current</c> and <c>PathComponent.Parent</c>.
    /// </returns>
    ///
    /// <remarks>
    /// A projection of the stored text, recomputed on demand. Cheap, total,
    /// and it keeps the original text authoritative. A resolution walk should use
    /// <c>PathCursor</c> instead, which is the same traversal without discarding
    /// where in the buffer it is (so you can also work out how symlinks affect resolution).
    /// </remarks>
    let components (path : UnixPath) : PathComponent list =
        let rec go (cursor : PathCursor) (acc : PathComponent list) : PathComponent list =
            match PathCursor.next cursor with
            | None -> List.rev acc
            | Some (component_, rest) -> go rest (component_ :: acc)

        go (PathCursor.ofPath path) []

    /// <summary>
    /// True when the path ended with a separator and named at least one
    /// component, e.g. "a/" or "/a/b/".
    /// </summary>
    /// <remarks>
    /// POSIX makes such a path equivalent to the same path with "/." appended,
    /// which forces the final component to resolve to a directory.
    /// So this cannot be normalised away without changing which paths succeed.
    /// </remarks>
    ///
    /// <returns>
    /// False for the root "/" (whose sole separator is the one that roots it,
    /// not a trailing one), for "//", and for the empty path.
    /// </returns>
    let hasTrailingSeparator (path : UnixPath) : bool =
        path.Raw.Length > 0
        && path.Raw.[path.Raw.Length - 1] = UnixPathText.separator
        && path.Raw |> String.exists (fun c -> c <> UnixPathText.separator)

    /// <summary>The empty path: neither rooted nor naming any component.</summary>
    /// <remarks>
    /// A legal C string, and one a guest really can pass, so it parses.
    /// However, no resolution of it can succeed, and the kernel reports <c>ENOENT</c> when you try.
    /// </remarks>
    let empty : UnixPath =
        {
            Raw = ""
        }

    /// <summary>True exactly when the input is the empty string.</summary>
    /// <remarks>
    /// Throws if the underlying string is null.
    ///
    /// This exists because POSIX generally requires APIs to return <c>ENOENT</c> for the empty path.
    /// </remarks>
    let isEmpty (path : UnixPath) : bool = path.Raw.Length = 0

    /// The root, "/".
    let root : UnixPath =
        {
            Raw = "/"
        }

    /// <summary>Parse a guest-supplied path.</summary>
    ///
    /// <remarks>
    /// Stores the candidate verbatim.
    /// (Even the simplest normalisation, collapsing consecutive separator characters, is observable on Darwin.)
    ///
    /// POSIX leaves a path beginning with exactly two separators
    /// implementation-defined (it may denote a distinct namespace).
    /// WoofWare.PosixKernel treats it as the root.
    /// </remarks>
    let parse (candidate : string) : Result<UnixPath, UnixPathError> =
        if isNull candidate then
            Error UnixPathError.Null
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (UnixPathError.Text defect)
        | None ->
            Ok
                {
                    Raw = candidate
                }

    /// Human-readable rendering of a rejection.
    let describe (error : UnixPathError) : string =
        match error with
        | UnixPathError.Null ->
            "path is null; the empty path is legal at this boundary, but a null one never reached the kernel at all"
        | UnixPathError.Text defect -> $"path %s{UnixPathText.describe defect}"

    /// <summary><c>UnixPath.parse</c>, but throwing on error.</summary>
    /// <remarks>Don't use this for paths arriving from a guest, because
    /// a guest can legally pass a path which contains NULs; the kernel must
    /// then reply to the guest with an error, rather than crashing.
    /// </remarks>
    let parseOrFail (context : string) (candidate : string) : UnixPath =
        match parse candidate with
        | Ok path -> path
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// <summary>
    /// Widen a fully-resolved absolute path into the shape of a guest-supplied path.
    /// </summary>
    let ofAbsolute (path : AbsoluteUnixPath) : UnixPath =
        let rendered = AbsoluteUnixPath.toString path

        match parse rendered with
        | Ok parsed -> parsed
        | Error error ->
            failwith
                $"UnixPath.ofAbsolute: %s{describe error} (got %s{rendered}). Every AbsoluteUnixPath satisfies UnixPath's invariant, so this value cannot have come from AbsoluteUnixPath.parse."
