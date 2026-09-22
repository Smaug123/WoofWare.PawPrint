namespace WoofWare.PosixKernel


/// <summary>
/// Why a candidate is not usable as a single Unix directory-entry name.
/// </summary>
[<RequireQualifiedAccess>]
type FileNameError =
    /// <summary>The candidate was null or empty.</summary>
    /// <remarks>
    /// No directory contains an entry with the empty name; a path that appears
    /// to ask for one ("a//b", "a/") has a zero-length <i>segment</i>, which
    /// <c>UnixPath.components</c> drops rather than turning into a name.
    /// </remarks>
    | Empty
    /// <summary>
    /// The candidate contained a separator at this byte index, so it names a path
    /// rather than a single entry within one directory.
    /// </summary>
    | ContainsSeparator of index : int
    /// <summary>The candidate could not survive the <c>char*</c> boundary.</summary>
    /// <remarks>
    /// See <c>UnixPathTextDefect</c>. Only <c>parse</c>, which takes a .NET string, reports this.
    /// </remarks>
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
/// This is non-empty, separator-free, NUL-free, and neither "." nor "..".
/// It need not be valid UTF-8.
///
/// Construct via <c>DirectoryEntryName.parse</c> or <c>DirectoryEntryName.ofByteString</c>.
/// </remarks>
[<Struct>]
type DirectoryEntryName =
    private
    | DirectoryEntryName of name : UnixByteString

    /// The name rendered for a diagnostic; see `UnixByteString.toEscaped`.
    override this.ToString () : string =
        match this with
        | DirectoryEntryName name -> UnixByteString.toEscaped name

[<RequireQualifiedAccess>]
module DirectoryEntryName =
    /// The name's bytes, exactly as `readdir` would hand them back.
    let toByteString (name : DirectoryEntryName) : UnixByteString =
        match name with
        | DirectoryEntryName name -> name

    /// The name as a .NET string, or `None` if its bytes are not valid UTF-8.
    let tryToString (name : DirectoryEntryName) : string option =
        UnixByteString.tryToString (toByteString name)

    /// The name rendered for a diagnostic; see `UnixByteString.toEscaped`.
    let toEscaped (name : DirectoryEntryName) : string =
        UnixByteString.toEscaped (toByteString name)

    /// Take a byte string as a single directory-entry name, or explain why it is not one.
    ///
    /// Never reports `FileNameError.Text`: a `UnixByteString` has no text defects.
    let ofByteString (candidate : UnixByteString) : Result<DirectoryEntryName, FileNameError> =
        let bytes = UnixByteString.toBytes candidate

        if bytes.Length = 0 then
            Error FileNameError.Empty
        else

        let separatorIndex = bytes.IndexOf UnixPathText.separatorByte

        if separatorIndex >= 0 then
            Error (FileNameError.ContainsSeparator separatorIndex)
        elif bytes.Length = 1 && bytes.[0] = 46uy then
            Error (FileNameError.Reserved ".")
        elif bytes.Length = 2 && bytes.[0] = 46uy && bytes.[1] = 46uy then
            Error (FileNameError.Reserved "..")
        else
            Ok (DirectoryEntryName candidate)

    /// <summary>Parse a single directory-entry name, or explain why the candidate is not one.</summary>
    /// <remarks>
    /// The name is the UTF-8 encoding of <c>candidate</c>.
    /// </remarks>
    let parse (candidate : string) : Result<DirectoryEntryName, FileNameError> =
        if System.String.IsNullOrEmpty candidate then
            Error FileNameError.Empty
        else

        match UnixByteString.ofString candidate with
        | Error defect -> Error (FileNameError.Text defect)
        | Ok bytes -> ofByteString bytes

    /// <summary>
    /// Human-readable rendering of a rejection.
    /// </summary>
    let describe (error : FileNameError) : string =
        match error with
        | FileNameError.Empty -> "name is null or empty, but no directory holds an entry with the empty name"
        | FileNameError.ContainsSeparator index ->
            $"name contains '%c{UnixPathText.separator}' at byte index %d{index}, so it is a path rather than a single entry name"
        | FileNameError.Text defect -> $"name %s{UnixPathText.describe defect}"
        | FileNameError.Reserved name ->
            $"\"%s{name}\" is a path component, not an entry name; PawPrint derives it from the directory graph rather than storing it"

    /// <summary>Parse a single directory-entry name, or throw.</summary>
    /// <remarks>This is <c>FileName.parse</c> except it throws instead of describing the error as a Result.</remarks>
    let parseOrFail (context : string) (candidate : string) : DirectoryEntryName =
        match parse candidate with
        | Ok name -> name
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// <summary>
    /// Re-check the invariant of a value that may not have come from <c>parse</c>,
    /// throwing (with a message containing <c>context</c>) if it does not hold.
    /// </summary>
    ///
    /// <remarks>
    /// The <c>parse</c> constructor already ensures the invariant holds, so there's
    /// no need to call this if you know the input came from <c>parse</c>.
    /// </remarks>
    let assertValid (context : string) (name : DirectoryEntryName) : DirectoryEntryName =
        // The only value this can reject is `Unchecked.defaultof` / C# `default`,
        // whose payload is a forged `UnixByteString`: `private` on the union case,
        // and the restrictions enforced by the constructors, stops every other route.
        match name with
        | DirectoryEntryName raw ->

        let raw = UnixByteString.assertValid context raw

        match ofByteString raw with
        | Ok _ -> name
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A FileName that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with FileName.parse instead."

/// <summary>
/// One component of a guest-supplied path, between two separators.
/// </summary>
[<RequireQualifiedAccess>]
type PathComponent =
    /// <summary>".".</summary>
    /// <remarks>
    /// This is not a no-op as a component, because it adds the constraint that
    /// the preceding segment must be a directory.
    /// That is, "a/." is illegal to resolve when "a" is a regular file, but
    /// not when it's a directory.
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
    | Name of name : DirectoryEntryName

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
    /// The empty string is a legal C string and a guest can legally supply it to a syscall;
    /// the kernel only rejects it at resolution time (with <c>ENOENT</c>).
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
/// Construct via <c>UnixPath.parse</c> or <c>UnixPath.ofByteString</c>.
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
            Raw : UnixByteString
        }

/// <summary>
/// A position part-way through resolving a path, specifying a component or separator within the path.
/// </summary>
/// <remarks>
/// This is the shape a Unix kernel's own resolution state has, namely a
/// pathname buffer plus a pointer into it (XNU's <c>cn_pnbuf</c> and <c>ni_next</c>,
/// Linux's <c>nameidata</c>).
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
            Buffer : UnixByteString
            /// <summary>
            /// How far through <c>Buffer</c> the walk has got, in bytes.
            /// </summary>
            /// <remarks>
            /// Always at a separator or at the end, never inside a component.
            /// </remarks>
            Offset : int
        }

[<RequireQualifiedAccess>]
module PathCursor =
    /// <summary>
    /// Begin walking a path, at its first component.
    /// </summary>
    let ofPath (path : UnixPath) : PathCursor =
        {
            Buffer = path.Raw
            Offset = 0
        }

    /// The bytes this cursor is walking. Throws if the cursor is a forged default.
    let private bufferOf (cursor : PathCursor) : System.Collections.Immutable.ImmutableArray<byte> =
        UnixByteString.assertValid "PathCursor (obtain one from PathCursor.ofPath)" cursor.Buffer
        |> UnixByteString.toBytes

    /// <summary>
    /// Where the next component starts, or the end of the buffer if only
    /// separators remain.
    /// </summary>
    let private afterSeparators (cursor : PathCursor) : int =
        let buffer = bufferOf cursor
        let mutable index = cursor.Offset

        while index < buffer.Length && buffer.[index] = UnixPathText.separatorByte do
            index <- index + 1

        index

    /// <summary>True when no component remains to be looked up.</summary>
    /// <remarks>A buffer holding only separators is exhausted: "a///" names only one component, not four.</remarks>
    let isExhausted (cursor : PathCursor) : bool =
        afterSeparators cursor = (bufferOf cursor).Length

    /// <summary>The next component, and the cursor positioned after it.</summary>
    /// <returns>
    /// <c>None</c> when the path is exhausted;
    /// otherwise, the component that was at the cursor, and a new cursor which is advanced to the next component.
    /// </returns>
    /// <remarks>Throws if the input cursor is a forged default.</remarks>
    let next (cursor : PathCursor) : (PathComponent * PathCursor) option =
        let buffer = bufferOf cursor
        let start = afterSeparators cursor

        if start = buffer.Length then
            None
        else

        let mutable finish = start

        while finish < buffer.Length && buffer.[finish] <> UnixPathText.separatorByte do
            finish <- finish + 1

        let segment = UnixByteString.slice start (finish - start) cursor.Buffer

        let component_ =
            match DirectoryEntryName.ofByteString segment with
            | Ok name -> PathComponent.Name name
            | Error (FileNameError.Reserved ".") -> PathComponent.Current
            | Error (FileNameError.Reserved "..") -> PathComponent.Parent
            | Error error ->
                // Every other rule has already been discharged: the segment is
                // non-empty (the scan above stopped at a non-separator) and holds
                // no separator (the scan stopped at one).
                failwith
                    $"PathCursor.next: segment \"%s{UnixByteString.toEscaped segment}\" of \"%s{UnixByteString.toEscaped cursor.Buffer}\" was rejected as an entry name (%s{DirectoryEntryName.describe error}), but a separator-free, non-empty segment can only be a name, \".\" or \"..\""

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
              && buffer.[niNext] = UnixPathText.separatorByte
              && (niNext + 1 = buffer.Length || buffer.[niNext + 1] = UnixPathText.separatorByte) do
            niNext <- niNext + 1

        Some (
            component_,
            { cursor with
                Offset = niNext
            }
        )

    /// <summary>How many bytes of the path there still are in front of the walk, <i>not</i>
    /// counting the NUL a kernel keeps at the end of its buffer.
    /// </summary>
    /// <remarks>
    /// XNU's <c>ni_pathlen</c> is this plus one.
    ///
    /// We don't expose a count of remaining <i>characters</i>, because this method is designed
    /// for use when computing kernel responses;
    /// a kernel's pathname buffer is bytes and its limits are byte counts.
    /// </remarks>
    let remainingBytes (cursor : PathCursor) : int =
        (bufferOf cursor).Length - cursor.Offset

    /// <summary>
    /// Expand the path assuming the cursor's current position marks a symlink to the given <c>target</c>,
    /// by appending to that target whatever the cursor had left to resolve.
    /// </summary>
    /// <remarks>
    /// Call <c>SymlinkTarget.toUnixPath</c> to construct the input target.
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
        // byte — so offset zero is exactly the condition to check.
        if cursor.Offset = 0 then
            failwith
                $"PathCursor.splice: the cursor into \"%s{UnixByteString.toEscaped cursor.Buffer}\" has not consumed a component, so there is no symbolic link here to expand. Splice onto the cursor `next` returned, not one straight from `ofPath`."

        let remainder =
            UnixByteString.slice cursor.Offset (buffer.Length - cursor.Offset) cursor.Buffer

        {
            Buffer = UnixByteString.append target.Raw remainder
            Offset = 0
        }

[<RequireQualifiedAccess>]
module UnixPath =
    /// The path's bytes, exactly as the guest passed them, separator runs and all.
    let toByteString (path : UnixPath) : UnixByteString = path.Raw

    /// The path as a .NET string, or `None` if its bytes are not valid UTF-8.
    let tryToString (path : UnixPath) : string option = UnixByteString.tryToString path.Raw

    /// The path rendered for a diagnostic; see `UnixByteString.toEscaped`.
    let toEscaped (path : UnixPath) : string = UnixByteString.toEscaped path.Raw

    /// <summary>True when the path began with a separator.</summary>
    /// <remarks>
    /// That is, path resolution for this path starts at the
    /// filesystem root rather than at a caller-supplied directory.
    /// </remarks>
    let isRooted (path : UnixPath) : bool =
        let bytes = UnixByteString.toBytes path.Raw
        bytes.Length > 0 && bytes.[0] = UnixPathText.separatorByte

    /// <summary>
    /// The path's components in order.
    /// </summary>
    /// <returns>
    /// No zero-length segments, but "." and ".." are preserved
    /// as <c>PathComponent.Current</c> and <c>PathComponent.Parent</c>.
    /// </returns>
    ///
    /// <remarks>
    /// A projection of the stored text, recomputed on demand.
    /// A resolution walk should use <c>PathCursor</c> instead,
    /// which is the same traversal without discarding where in the buffer it is
    /// (so you can also work out how symlinks affect resolution).
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
    /// So this cannot be normalised away without changing which paths succeed resolution.
    /// </remarks>
    ///
    /// <returns>
    /// False for the root "/" (whose sole separator is the one that roots it,
    /// not a trailing one), for "//" (or "/////" etc), and for the empty path.
    /// </returns>
    let hasTrailingSeparator (path : UnixPath) : bool =
        let bytes = UnixByteString.toBytes path.Raw

        bytes.Length > 0
        && bytes.[bytes.Length - 1] = UnixPathText.separatorByte
        && bytes |> Seq.exists (fun b -> b <> UnixPathText.separatorByte)

    /// <summary>The empty path: neither rooted nor naming any component.</summary>
    /// <remarks>
    /// A legal C string, and one a guest really can pass, so it parses.
    /// However, no resolution of it can succeed, and the kernel reports <c>ENOENT</c> when you try.
    /// </remarks>
    let empty : UnixPath =
        {
            Raw = UnixByteString.empty
        }

    /// <summary>True exactly when the input is the empty string.</summary>
    /// <remarks>
    /// This exists because POSIX generally requires APIs to return <c>ENOENT</c> for the empty path.
    /// </remarks>
    let isEmpty (path : UnixPath) : bool = UnixByteString.length path.Raw = 0

    /// Take a guest-supplied byte string as a path. Every NUL-free byte
    /// string is one, so this cannot fail.
    ///
    /// Stores the bytes verbatim, for the reasons `parse` gives.
    let ofByteString (bytes : UnixByteString) : UnixPath =
        {
            Raw = UnixByteString.assertValid "UnixPath.ofByteString" bytes
        }

    /// <summary>
    /// The root, "/".
    /// </summary>
    let root : UnixPath =
        match UnixByteString.ofString "/" with
        | Ok bytes -> ofByteString bytes
        | Error defect -> failwith $"UnixPath.root: %s{UnixPathText.describe defect}"

    /// <summary>Parse a guest-supplied path.</summary>
    ///
    /// <remarks>
    /// Stores the UTF-8 encoding of the candidate verbatim.
    /// (Even the simplest normalisation, collapsing consecutive separator characters, is observable on Darwin,
    /// so we can't even do that during parse.)
    ///
    /// POSIX leaves a path beginning with exactly two separators
    /// implementation-defined (it may denote a distinct namespace).
    /// WoofWare.PosixKernel treats it as the root.
    /// </remarks>
    let parse (candidate : string) : Result<UnixPath, UnixPathError> =
        if isNull candidate then
            Error UnixPathError.Null
        else

        match UnixByteString.ofString candidate with
        | Error defect -> Error (UnixPathError.Text defect)
        | Ok bytes -> Ok (ofByteString bytes)

    /// <summary>
    /// Human-readable rendering of a rejection.
    /// </summary>
    let describe (error : UnixPathError) : string =
        match error with
        | UnixPathError.Null ->
            "path is null; the empty path is legal at this boundary, but a null one never reached the kernel at all"
        | UnixPathError.Text defect -> $"path %s{UnixPathText.describe defect}"

    /// <summary><c>UnixPath.parse</c>, but throwing on error.</summary>
    /// <remarks>
    /// Don't use this for paths arriving from a guest, because
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
        ofByteString (AbsoluteUnixPath.toByteString path)
