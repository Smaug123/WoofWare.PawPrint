namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Why a string is not usable as a single Unix directory-entry name.
[<RequireQualifiedAccess>]
type FileNameError =
    /// The candidate was null or empty. No directory contains an entry with
    /// the empty name; a path that appears to ask for one ("a//b", "a/") has a
    /// zero-length *segment*, which `UnixPath.components` drops rather than
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

    /// Re-check the invariant of a value that may not have come from `parse`.
    /// Returns it unchanged if it is sound, and fails loudly naming `context`
    /// if it is not.
    ///
    /// Only for boundaries that accept a `FileName` from outside the library.
    /// The only value this can reject is `Unchecked.defaultof` / C# `default`,
    /// whose payload is null; catching it turns a name no parsed path could
    /// ever produce into an error at the point it enters the model, rather than
    /// a directory entry that `checkInvariants` happily calls sound. Interior
    /// consumers must *not* call this: re-validating a proof everywhere is
    /// precisely what the type exists to avoid.
    let assertValid (context : string) (name : FileName) : FileName =
        match name with
        | FileName raw ->

        match parse raw with
        | Ok _ -> name
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A FileName that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with FileName.parse instead."

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
///
/// Held **verbatim**, as `SymlinkTarget` is, rather than as a parsed
/// decomposition: the structure is recovered on demand by `UnixPath.components`
/// and `PathCursor`, and the stored text stays authoritative.
///
/// A kernel resolves a path out of a byte buffer,
/// and Darwin's length rules count the bytes *in that buffer* — so "a//b" and
/// "a/b" are behaviourally distinct on a real kernel (measured: a symlink
/// splice with an "/a//b" remainder refuses a target one byte shorter than the
/// same splice with "/a/b"). A representation that collapsed separator runs
/// would make two distinguishable paths equal, and could not recover the count
/// afterwards.
type UnixPath =
    private
        {
            /// The path exactly as it was handed to the syscall: separator runs
            /// and trailing separators intact, "." and ".." uninterpreted.
            ///
            /// Every rule `parse` enforces holds of it — non-null, no embedded
            /// NUL, no unpaired surrogate — and nothing else does.
            Raw : string
        }

/// A position part-way through resolving a path: the text still being walked,
/// and how far through it the walk has got.
///
/// This is deliberately the shape a Unix kernel's own resolution state has — a
/// pathname buffer plus a pointer into it (XNU's `cn_pnbuf` and `ni_next`,
/// Linux's `nameidata`). Expanding a symbolic link *replaces the buffer* rather
/// than editing a list of components, which is what makes the byte counts the
/// kernel compares recoverable at all.
[<Struct>]
type PathCursor =
    private
        {
            /// The path text currently being resolved. Not necessarily the text
            /// the guest passed: each symbolic link expansion replaces it.
            ///
            /// Always satisfies `UnixPath`'s invariant, because every way to
            /// build one takes a `UnixPath`.
            Buffer : string
            /// How far through `Buffer` the walk has got. Always at a separator
            /// or at the end, never inside a component.
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

    /// The text this cursor is walking, having checked that the cursor is not a
    /// forged default.
    ///
    /// This type is a struct, so `Unchecked.defaultof` and C# `default` produce
    /// a value that never went through `ofPath` and whose buffer is null. Every
    /// function here reads the buffer through this one accessor, so none of them
    /// can forget to check — and the failure is a named crash rather than a
    /// `NullReferenceException` from inside a scan, or (worse) a silent
    /// impersonation of the empty path, which resolves as "the directory I
    /// started from" rather than the ENOENT the empty path owes its caller.
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

    /// True when no component remains to be looked up. A buffer holding only
    /// separators is exhausted: "a///" names one component, not four.
    let isExhausted (cursor : PathCursor) : bool =
        afterSeparators cursor = (bufferOf cursor).Length

    /// The next component, and the cursor positioned after it — or `None` when
    /// the path is exhausted. Total for every cursor `ofPath` or `splice`
    /// produced; a forged default is refused loudly rather than walked.
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

    /// How many bytes of pathname the walk still has in front of it, **not**
    /// counting the NUL a kernel keeps at the end of its buffer. XNU's
    /// `ni_pathlen` is this plus one.
    ///
    /// Bytes, not characters: a kernel's pathname buffer is bytes and its
    /// limits are byte counts. Measured on Darwin 25.6.0, with U+4E2D (three
    /// UTF-8 bytes, one UTF-16 code unit): a symlink target of 1022 raw bytes
    /// spelled in CJK is refused where 1019 resolves, i.e. the budget tracks
    /// bytes and not `String.Length`. The *other* limit next door —
    /// `PathLimits.nameWithinLimit` — does count UTF-16 code units on Darwin,
    /// so the wrong function is right often enough to look correct.
    let remainingBytes (cursor : PathCursor) : int =
        UnixPathText.utf8.GetByteCount (remainder cursor)

    /// Expand a symbolic link: the target, followed by whatever the walk had
    /// left to resolve. This is what a kernel does to its pathname buffer, and
    /// the reason the buffer is a buffer.
    ///
    /// Takes a `UnixPath` rather than a string so that the result satisfies
    /// `Buffer`'s invariant by construction; `SymlinkTarget.toUnixPath` is how
    /// a stored target becomes one.
    ///
    /// The cursor must be one `next` handed back. Expanding a link *replaces the
    /// component just consumed*, so a cursor that has consumed nothing has
    /// nothing to replace: joining a target onto it would run the target's last
    /// component into the path's first, and splicing "l" onto "abc" would look
    /// up "labc". `next` never returns offset zero — a component is at least one
    /// character — so that is exactly the condition checked here.
    let splice (target : UnixPath) (cursor : PathCursor) : PathCursor =
        // Validity first: a forged default has offset zero too, and would
        // otherwise be reported as the wrong mistake.
        let buffer = bufferOf cursor

        if cursor.Offset = 0 then
            failwith
                $"PathCursor.splice: the cursor into \"%s{buffer}\" has not consumed a component, so there is no symbolic link here to expand. Splice onto the cursor `next` returned, not one straight from `ofPath`."

        {
            Buffer = target.Raw + remainder cursor
            Offset = 0
        }

[<RequireQualifiedAccess>]
module UnixPath =
    /// Render back to the string a guest would have passed. Exact: this returns
    /// the very text `parse` accepted, separator runs and all, so
    /// `toString (parse s) = s` for every `s` that parses.
    let toString (path : UnixPath) : string = path.Raw

    /// True when the path began with a separator, so resolution starts at the
    /// filesystem root rather than at a caller-supplied directory.
    let isRooted (path : UnixPath) : bool =
        path.Raw.Length > 0 && path.Raw.[0] = UnixPathText.separator

    /// The path's components in order: no zero-length segments, but "." and
    /// ".." preserved as `PathComponent.Current` and `PathComponent.Parent`.
    ///
    /// A projection of the stored text, recomputed on demand — cheap, total,
    /// and it keeps the text authoritative. A resolution walk should use
    /// `PathCursor` instead, which is the same traversal without discarding
    /// where in the buffer it is.
    let components (path : UnixPath) : PathComponent list =
        let rec go (cursor : PathCursor) (acc : PathComponent list) : PathComponent list =
            match PathCursor.next cursor with
            | None -> List.rev acc
            | Some (component_, rest) -> go rest (component_ :: acc)

        go (PathCursor.ofPath path) []

    /// True when the path ended with a separator and named at least one
    /// component, e.g. "a/" or "/a/b/". POSIX makes such a path equivalent to
    /// the same path with "/." appended, which forces the final component to
    /// resolve to a directory — so this cannot be normalised away without
    /// changing which paths succeed.
    ///
    /// False for the root "/" (whose sole separator is the one that roots it,
    /// not a trailing one), for "//", and for the empty path.
    let hasTrailingSeparator (path : UnixPath) : bool =
        path.Raw.Length > 0
        && path.Raw.[path.Raw.Length - 1] = UnixPathText.separator
        && path.Raw |> String.exists (fun c -> c <> UnixPathText.separator)

    /// The empty path: neither rooted nor naming any component. A legal C
    /// string, and one a guest really can pass, so it parses — but no
    /// resolution of it can succeed, and the kernel reports ENOENT.
    let empty : UnixPath =
        {
            Raw = ""
        }

    /// True for the one path that names nothing at all. Callers resolving a
    /// path must check this first: POSIX requires ENOENT for the empty path,
    /// which is *not* what a walk over zero components would otherwise
    /// produce (that would silently mean "the directory I started from").
    ///
    /// Only the empty text qualifies: any other path either begins with a
    /// separator, and so is rooted, or names a component.
    let isEmpty (path : UnixPath) : bool = path.Raw.Length = 0

    /// The root, "/".
    let root : UnixPath =
        {
            Raw = "/"
        }

    /// Parse a guest-supplied path. Total: never throws, for any input
    /// including null.
    ///
    /// Stores the candidate **verbatim**. Nothing is normalised — not repeated
    /// separators, not a trailing separator, not "." or ".." — because a real
    /// kernel resolves the bytes it was given and measures their length, so
    /// every one of those is observable. The structure is recovered by
    /// `components` and `PathCursor`, which collapse separator runs exactly
    /// where the kernel does.
    ///
    /// What it *does* enforce is the boundary invariant: the text must be able
    /// to survive as a C string (see `UnixPathText`). That is what makes the
    /// projections total.
    ///
    /// POSIX leaves a path beginning with exactly two separators
    /// implementation-defined (it may denote a distinct namespace). Every Unix
    /// PawPrint models treats it as the root, and so does this.
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

    /// Parse, or fail loudly naming the boundary at fault. For callers with no
    /// way to recover; a path arriving from a *guest* must not use this, since
    /// a guest passing a NUL-bearing path is a runtime condition the kernel
    /// answers with an error, not a host bug.
    let parseOrFail (context : string) (candidate : string) : UnixPath =
        match parse candidate with
        | Ok path -> path
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

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
