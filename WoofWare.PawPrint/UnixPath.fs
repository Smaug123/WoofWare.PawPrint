namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Why a string is not usable as a single Unix directory-entry name.
[<RequireQualifiedAccess>]
type FileNameError =
    /// The candidate was null or empty. No directory contains an entry with
    /// the empty name; a path that appears to ask for one ("a//b", "a/") has a
    /// zero-length *segment*, which `UnixPath.parse` collapses rather than
    /// turning into a name.
    | Empty
    /// The candidate contained a separator at this index, so it names a path
    /// rather than a single entry within one directory.
    | ContainsSeparator of index : int
    /// The candidate could not survive the `char*` boundary; see
    /// `UnixPathTextDefect`.
    | Text of defect : UnixPathTextDefect
    /// The candidate was "." or "..". Both are legal path *components*, and
    /// `PathComponent` carries them as their own cases — but neither is ever
    /// the name of an entry a directory holds in PawPrint's model, which
    /// derives both from the directory graph rather than storing them.
    | Reserved of name : string

/// A single component of a Unix path that names an actual directory entry:
/// non-empty, separator-free, NUL-free, UTF-8-encodable, and neither "." nor
/// "..".
///
/// Construct via `FileName.parse`. Keeping this distinct from `string` is what
/// stops a caller from binding "a/b" or ".." as a directory entry, which would
/// make the inode graph describe a filesystem no kernel could produce.
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
    let toString (name : FileName) : string =
        match name with
        | FileName name -> name

    /// Parse a single directory-entry name, or explain why the candidate is
    /// not one. Total: never throws, for any input including null.
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

    /// Parse, or fail loudly naming the boundary at fault. For callers with no
    /// way to recover — a bad literal in PawPrint's own source, or a
    /// malformed seed manifest, is a host bug rather than a runtime condition.
    let parseOrFail (context : string) (candidate : string) : FileName =
        match parse candidate with
        | Ok name -> name
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// The name as the NUL-free byte string a Unix kernel would hand back from
    /// `readdir`. Without a terminator; callers that need a C string append
    /// the NUL themselves.
    let toUtf8 (name : FileName) : ImmutableArray<byte> =
        toString name |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange

/// One component of a guest-supplied path, between two separators.
///
/// A DU rather than a string because the three cases mean structurally
/// different things to a resolution walk — `Current` and `Parent` are
/// navigation, `Name` is a lookup — and because the kernel reports different
/// errors for them in the final position (`mkdir("a/..")` is EEXIST, not a
/// request to create an entry called "..").
[<RequireQualifiedAccess>]
type PathComponent =
    /// ".". Not a no-op: it constrains what precedes it to be a directory, so
    /// "a/." and "a" resolve differently when "a" is a regular file.
    | Current
    /// "..". Resolved against the *physical* parent recorded in the directory
    /// graph, never against the lexical predecessor in the path — after a walk
    /// crosses a symlink, the two differ, and only the physical answer matches
    /// the kernel.
    | Parent
    /// An ordinary entry name, to be looked up in the directory reached so far.
    | Name of name : FileName

/// Why a string is not usable as a guest-supplied Unix path.
///
/// Far more permissive than `AbsoluteUnixPathError`: a guest may legitimately
/// pass a relative path, repeated separators, a trailing separator, and "." or
/// ".." components, all of which the kernel accepts. Only the two boundary
/// rules remain.
[<RequireQualifiedAccess>]
type UnixPathError =
    /// The candidate was null. Distinct from the *empty* path, which is a
    /// legal C string that the kernel rejects with ENOENT at resolution time
    /// rather than at parse time — so `parse ""` succeeds.
    | Null
    /// The candidate could not survive the `char*` boundary; see
    /// `UnixPathTextDefect`.
    | Text of defect : UnixPathTextDefect

/// A path exactly as a guest handed it to a syscall: possibly relative,
/// possibly containing "." and ".." components, possibly with a trailing
/// separator. This is the shape every `SystemNative_*` path argument takes,
/// and the input to the `VirtualFileSystem` resolution walk.
///
/// Contrast `AbsoluteUnixPath`, which is the strictly narrower shape
/// `getcwd(3)` can *return*: rooted, fully resolved, no trailing separator.
/// Every `AbsoluteUnixPath` is a `UnixPath` (see `UnixPath.ofAbsolute`); the
/// converse holds only for paths a resolution walk has already reduced.
///
/// Construct via `UnixPath.parse`.
type UnixPath =
    private
        {
            /// True when the path began with a separator, so resolution starts
            /// at the filesystem root rather than at a directory the caller
            /// supplies.
            IsRooted : bool
            /// The path's components in order, with zero-length segments
            /// (produced by repeated separators) already dropped. "." and ".."
            /// are *not* dropped: both are load-bearing during resolution.
            Components : PathComponent list
            /// True when the path ended with a separator and named at least one
            /// component, e.g. "a/" or "/a/b/". POSIX makes such a path
            /// equivalent to the same path with "/." appended, which forces the
            /// final component to resolve to a directory — so this cannot be
            /// normalised away without changing which paths succeed.
            ///
            /// False for the root "/" (whose sole separator is the one that
            /// roots it, not a trailing one) and for the empty path.
            HasTrailingSeparator : bool
        }

[<RequireQualifiedAccess>]
module UnixPath =
    /// True when the path began with a separator, so resolution starts at the
    /// filesystem root rather than at a caller-supplied directory.
    let isRooted (path : UnixPath) : bool = path.IsRooted

    /// The path's components in order: no zero-length segments, but "." and
    /// ".." preserved as `PathComponent.Current` and `PathComponent.Parent`.
    let components (path : UnixPath) : PathComponent list = path.Components

    /// True when the path ended with a separator and named at least one
    /// component. See the field's own documentation for why this survives
    /// parsing.
    let hasTrailingSeparator (path : UnixPath) : bool = path.HasTrailingSeparator

    /// The empty path: neither rooted nor naming any component. A legal C
    /// string, and one a guest really can pass, so it parses — but no
    /// resolution of it can succeed, and the kernel reports ENOENT.
    let empty : UnixPath =
        {
            IsRooted = false
            Components = []
            HasTrailingSeparator = false
        }

    /// True for the one path that names nothing at all. Callers resolving a
    /// path must check this first: POSIX requires ENOENT for the empty path,
    /// which is *not* what a walk over zero components would otherwise
    /// produce (that would silently mean "the directory I started from").
    let isEmpty (path : UnixPath) : bool =
        not path.IsRooted && List.isEmpty path.Components

    /// The root, "/".
    let root : UnixPath =
        {
            IsRooted = true
            Components = []
            HasTrailingSeparator = false
        }

    /// Parse a guest-supplied path. Total: never throws, for any input
    /// including null.
    ///
    /// Deliberately *normalises* only what the kernel itself normalises —
    /// zero-length segments, which arise from repeated separators and which
    /// every Unix collapses. It does not resolve "." or "..", because
    /// resolving ".." lexically is only correct in the absence of symlinks;
    /// that is the resolution walk's job, against the real directory graph.
    ///
    /// Note POSIX leaves a path beginning with exactly two separators
    /// implementation-defined (it may denote a distinct namespace). Every Unix
    /// PawPrint models treats it as the root, and so does this.
    let parse (candidate : string) : Result<UnixPath, UnixPathError> =
        if isNull candidate then
            Error UnixPathError.Null
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (UnixPathError.Text defect)
        | None ->

        let isRooted = candidate.Length > 0 && candidate.[0] = UnixPathText.separator

        let segments =
            candidate.Split UnixPathText.separator
            |> Array.filter (fun segment -> segment.Length > 0)

        let components =
            segments
            |> Array.map (fun segment ->
                match segment with
                | "." -> PathComponent.Current
                | ".." -> PathComponent.Parent
                | _ ->
                    // Every rule `FileName.parse` enforces has already been
                    // discharged: the segment is non-empty (filtered), holds no
                    // separator (it came from a split on one), carries no text
                    // defect (the whole candidate was scanned), and is neither
                    // "." nor ".." (matched above).
                    match FileName.parse segment with
                    | Ok name -> PathComponent.Name name
                    | Error error ->
                        failwith
                            $"UnixPath.parse: segment \"%s{segment}\" of \"%s{candidate}\" was rejected as an entry name (%s{FileName.describe error}), but parsing has already excluded every reason a segment could be rejected"
            )
            |> Array.toList

        // The final separator of "/" is the one that roots it, so it is not a
        // trailing separator; likewise there is nothing to trail in "" or "//".
        let hasTrailingSeparator =
            not (List.isEmpty components)
            && candidate.[candidate.Length - 1] = UnixPathText.separator

        Ok
            {
                IsRooted = isRooted
                Components = components
                HasTrailingSeparator = hasTrailingSeparator
            }

    /// Human-readable rendering of a rejection.
    let describe (error : UnixPathError) : string =
        match error with
        | UnixPathError.Null ->
            "path is null; the empty path is legal at this boundary, but a null one never reached the kernel at all"
        | UnixPathError.Text defect -> $"path %s{UnixPathText.describe defect}"

    /// Parse, or fail loudly naming the boundary at fault. For callers with no
    /// way to recover; a path arriving from a *guest* must not use this, since
    /// a guest passing a NUL-bearing path is a runtime condition the kernel
    /// answers with an error, not a host bug.
    let parseOrFail (context : string) (candidate : string) : UnixPath =
        match parse candidate with
        | Ok path -> path
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// Render back to the string a guest would have passed. Round-trips:
    /// `parse (toString p) = Ok p` for every `p` that `parse` produced.
    ///
    /// This is *not* the inverse of `parse` on strings — the normalisation
    /// `parse` performs is deliberately lossy, so "a//b" renders back as
    /// "a/b". It is the inverse on the normalised image, which is what makes
    /// it usable for diagnostics without misreporting what will be resolved.
    let toString (path : UnixPath) : string =
        let rendered =
            path.Components
            |> List.map (fun component_ ->
                match component_ with
                | PathComponent.Current -> "."
                | PathComponent.Parent -> ".."
                | PathComponent.Name name -> FileName.toString name
            )
            |> String.concat (string UnixPathText.separator)

        let prefix = if path.IsRooted then string UnixPathText.separator else ""

        let suffix =
            if path.HasTrailingSeparator then
                string UnixPathText.separator
            else
                ""

        prefix + rendered + suffix

    /// Widen a fully-resolved absolute path into the guest-supplied shape, for
    /// callers that need to resolve one — notably the current directory, which
    /// is where every relative path a guest passes starts from.
    ///
    /// Total: `AbsoluteUnixPath`'s invariant is strictly stronger than
    /// `UnixPath`'s, so this cannot fail. The `failwith` asserts that, and can
    /// only fire for a forged value (see `AbsoluteUnixPath.assertValid`).
    let ofAbsolute (path : AbsoluteUnixPath) : UnixPath =
        let rendered = AbsoluteUnixPath.toString path

        match parse rendered with
        | Ok parsed -> parsed
        | Error error ->
            failwith
                $"UnixPath.ofAbsolute: %s{describe error} (got %s{rendered}). Every AbsoluteUnixPath satisfies UnixPath's invariant, so this value cannot have come from AbsoluteUnixPath.parse."

    /// The path obtained by resolving `relative` against `basePath`: `relative`
    /// itself when it is rooted, and otherwise `basePath`'s components followed
    /// by `relative`'s.
    ///
    /// This is *lexical* concatenation, not resolution — no "." or ".."
    /// component is interpreted, and no symlink is followed. It exists for the
    /// resolution walk to splice a symlink's target into the path it is
    /// walking, which is exactly the operation POSIX describes.
    let concat (basePath : UnixPath) (relative : UnixPath) : UnixPath =
        if relative.IsRooted then
            relative
        else

        {
            IsRooted = basePath.IsRooted
            Components = basePath.Components @ relative.Components
            // A separator between the two halves absorbs any that trailed the
            // base, so only the suffix's own trailing separator survives —
            // unless the suffix contributed nothing, in which case the base's
            // does. ("a/" ++ "b" is "a/b"; "a/" ++ "" is "a/".)
            HasTrailingSeparator =
                if List.isEmpty relative.Components then
                    basePath.HasTrailingSeparator
                else
                    relative.HasTrailingSeparator
        }
