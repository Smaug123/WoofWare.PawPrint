namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// The longest a single path component may be, *and the unit that length is
/// measured in* — which is not the same on every Unix, so the two travel
/// together as one value rather than as a number beside a unit that could
/// disagree with it.
///
/// Both platforms say "255". Measured, they mean different things by it:
///
/// | name | UTF-8 bytes | UTF-16 units | APFS | ext4 |
/// |---|---|---|---|---|
/// | `a`×255 / `a`×256 | 255 / 256 | 255 / 256 | ok / too long | ok / too long |
/// | `中`×85 / `中`×86 | 255 / 258 | 85 / 86 | ok / ok | **ok / too long** |
/// | `中`×255 | 765 | 255 | **ok** | **too long** |
/// | emoji×127 + `a` | 509 | 255 | **ok** | too long |
/// | emoji×127 + `aa` | 510 | 256 | **too long** | too long |
///
/// The last two rows are what separate "UTF-16 code units" from "characters":
/// an emoji is one character but two units, and APFS's boundary tracks the
/// units. APFS also counts the name *as given* — `é`×255 in NFC is permitted,
/// where counting its NFD expansion would give 510 units and refuse it.
///
/// This is really a property of the *filesystem* rather than the kernel
/// (`_PC_NAME_MAX` varies per mount on Linux too); PawPrint models one
/// filesystem per flavour, which is the same simplification it makes elsewhere.
/// A struct, deliberately: `PathLimits` is one too, so a reference DU here would
/// make a forged `Unchecked.defaultof<PathLimits>` carry a *null* limit, and
/// `assertValid` would then have to match on it to reject it — which is exactly
/// the operation that would throw. As a struct the forged default is
/// `Utf8Bytes 0`, which reads as an ordinary case carrying a zero, and the zero
/// is what `assertValid` rejects.
[<RequireQualifiedAccess>]
[<Struct>]
type NameLengthLimit =
    /// <summary>
    /// The length limit is this number of UTF8 bytes.
    /// </summary>
    /// <example>
    /// Linux filesystems generally, and ext4 specifically, use this limit:
    /// a raw byte count, which is what the kernel stores and compares.
    /// </example>
    | Utf8Bytes of bytes : int
    /// <summary>
    /// The length limit is this number of UTF-16 code units.
    /// </summary>
    /// <example>
    /// APFS (and HFS+ before it) use this limit.
    /// Those filesystems store names as UTF-16, and express bounds as a count
    /// of code units.
    /// </example>
    /// <remarks>
    /// Beware: .NET measures strings in UTF-16 code units, so will coincidentally get the right
    /// answer on APFS if you use its <c>String.Length</c> instead of computing the proper
    /// simulated kernel's answer!
    /// </remarks>
    | Utf16CodeUnits of units : int

/// <summary>
/// Whether expanding a symbolic link re-checks that the path still fits in
/// <c>PATH_MAX</c>.
/// </summary>
/// <remarks>
/// This is, surprisingly, platform-dependent: Linux will happily splice long
/// symlinks into a path and vastly exceed <c>PATH_MAX</c>.
/// </remarks>
(*
Measured, by bisecting the symlink-target length at which a dangling link
flips ENOENT → ENAMETOOLONG (Darwin 25.6.0 / macOS 26.6 and Linux 6.18.5).
Darwin refuses when `linklen + ni_pathlen > MAXPATHLEN` — XNU's `lookup`
splices by copying the target and the unconsumed remainder into a fresh
`MAXPATHLEN` buffer, so the rule is simply that the new buffer must fit.
Linux has no such check *at all*: measured, a 3842-byte target with an
806-byte remainder resolves at 4648 bytes spliced, well past its own
`PATH_MAX`.
*)
[<RequireQualifiedAccess>]
[<Struct>]
type SpliceLengthRecheck =
    /// <summary>
    /// After splicing, the spliced path (target bytes, unconsumed remainder, and trailing NUL)
    /// must still fit in <c>PATH_MAX</c>.
    /// </summary>
    /// <example>
    /// Darwin does this.
    /// </example>
    | Recheck
    /// <summary>
    /// A path may grow without bound as links are expanded, so long as
    /// each <i>component</i> is within <c>NAME_MAX</c> and the original argument was
    /// within <c>PATH_MAX</c>.
    /// </summary>
    /// <example>
    /// Linux does this.
    /// </example>
    | NoRecheck

/// The bounds a kernel puts on path resolution, which differ between the Unixes
/// PawPrint models and so cannot be constants in the walk.
///
/// A record rather than a bare `int`: `MAXSYMLINKS`, `PATH_MAX` and `NAME_MAX`
/// are the same kind of fact, and a caller that needs one generally needs the
/// others.
///
/// Not `SimulatedUnixPlatform` itself, which lives in `EmulatedKernel` and
/// compiles later — the same split `RawErrnoNumbering` makes, and for the same
/// reason: this file has no business knowing what a platform *is*, only that
/// something has chosen limits. `SimulatedUnixPlatform.pathLimits` is the
/// mapping, and is where the numbers are justified as measured facts about
/// real kernels.
///
/// Not a field of `VirtualFileSystem` either, which would have saved threading
/// it through every call. A `VirtualFileSystem` is a filesystem *image*: it
/// comes from a seed, which has no platform and could not sensibly acquire
/// one. `MAXSYMLINKS` is a property of the kernel doing the walking, not of
/// the tree being walked, and storing it in the image would let two
/// filesystems under one kernel disagree about it — a state no real system can
/// be in.
[<Struct>]
type PathLimits =
    private
        {
            /// How many symbolic links a single resolution may traverse before
            /// the kernel gives up with ELOOP. macOS's `MAXSYMLINKS` is 32
            /// (`sys/param.h`, and probed: a chain of 32 resolves, 33 gives
            /// ELOOP); Linux's is 40 (`MAXSYMLINKS` in `include/linux/namei.h`,
            /// a kernel-internal header rather than a UAPI one).
            MaxSymlinkTraversals : int
            /// The longest pathname the kernel will accept as a syscall
            /// *argument*, **including its NUL terminator** — so a usable path
            /// is one byte shorter. Darwin 1024, Linux 4096 (measured: an
            /// argument of 1023 bytes resolves on macOS and 1024 does not; 4095
            /// and 4096 respectively on Linux).
            ///
            /// Binds the argument as passed, *not* the resolved path: a
            /// 1023-byte relative path resolved from a long working directory is
            /// fine, and Linux will happily `chdir` into a tree 4250 bytes deep
            /// so long as it is built one component at a time. That is why this
            /// is enforced at the syscall boundary rather than in the walk.
            PathMaxBytes : int
            /// The longest single component, with its unit.
            ///
            /// Read only through `nameWithinLimit`, which is why there is no
            /// accessor for it: the number is meaningless without the unit, and
            /// a caller holding both could still measure with the wrong one.
            NameMax : NameLengthLimit
            /// Whether expanding a symbolic link re-checks the total length.
            /// Read only through `spliceWithinLimit`, for the same reason
            /// `NameMax` is read only through `nameWithinLimit`.
            SpliceRecheck : SpliceLengthRecheck
        }

[<RequireQualifiedAccess>]
module PathLimits =
    /// Fails rather than returning an option: unlike `PermissionBits.parse`,
    /// whose input is a guest's, every caller of this is the platform table
    /// passing a literal, so a bad value is an interpreter bug and not something
    /// a caller could handle.
    let create
        (maxSymlinkTraversals : int)
        (pathMaxBytes : int)
        (nameMax : NameLengthLimit)
        (spliceRecheck : SpliceLengthRecheck)
        : PathLimits
        =
        if maxSymlinkTraversals < 1 then
            failwith
                $"PathLimits.create: a kernel that permits %d{maxSymlinkTraversals} symlink traversals could not resolve a path through any symbolic link at all; every Unix PawPrint models permits at least one."

        // Two adjacent `int` parameters are an argument-order hazard, so the
        // bounds are chosen to make a swap a loud failure rather than a subtly
        // wrong kernel: no Unix has a PATH_MAX below 256 (POSIX's floor,
        // _POSIX_PATH_MAX, is 256) and none permits anywhere near 256 symlink
        // traversals, so `create 1024 32 ...` cannot pass both checks.
        if maxSymlinkTraversals > 255 then
            failwith
                $"PathLimits.create: %d{maxSymlinkTraversals} symlink traversals is far beyond any Unix PawPrint models (Linux permits 40, Darwin 32). Are the first two arguments the wrong way round?"

        if pathMaxBytes < 256 then
            failwith
                $"PathLimits.create: a PATH_MAX of %d{pathMaxBytes} bytes is below POSIX's _POSIX_PATH_MAX floor of 256. Are the first two arguments the wrong way round?"

        match nameMax with
        | NameLengthLimit.Utf8Bytes bytes when bytes < 1 ->
            failwith $"PathLimits.create: a NAME_MAX of %d{bytes} bytes would forbid every filename."
        | NameLengthLimit.Utf16CodeUnits units when units < 1 ->
            failwith $"PathLimits.create: a NAME_MAX of %d{units} UTF-16 code units would forbid every filename."
        | NameLengthLimit.Utf8Bytes _
        | NameLengthLimit.Utf16CodeUnits _ -> ()

        {
            MaxSymlinkTraversals = maxSymlinkTraversals
            PathMaxBytes = pathMaxBytes
            NameMax = nameMax
            SpliceRecheck = spliceRecheck
        }

    let maxSymlinkTraversals (limits : PathLimits) : int = limits.MaxSymlinkTraversals

    /// The longest pathname this kernel accepts as a syscall argument,
    /// *including* the NUL terminator — so a usable path is one byte shorter.
    let pathMaxBytes (limits : PathLimits) : int = limits.PathMaxBytes

    /// Whether a single path component is short enough for this kernel, measured
    /// in whichever unit that kernel counts in.
    ///
    /// The only way to read `NameMax`, on purpose. Handing out the number and
    /// the unit separately would let a caller measure a name with the wrong one
    /// — and on a Mac the wrong one (`String.Length`) is right often enough to
    /// look correct.
    let nameWithinLimit (limits : PathLimits) (name : DirectoryEntryName) : bool =
        match limits.NameMax with
        | NameLengthLimit.Utf8Bytes bytes -> UnixPathText.utf8.GetByteCount (DirectoryEntryName.toString name) <= bytes
        | NameLengthLimit.Utf16CodeUnits units -> (DirectoryEntryName.toString name).Length <= units

    /// Whether this kernel will still resolve the path that results from
    /// expanding `target` here — or, on a kernel that does not re-check,
    /// unconditionally true.
    ///
    /// The only way to read `SpliceRecheck`, on purpose, for the reason
    /// `nameWithinLimit` is the only way to read `NameMax`: the caller would
    /// otherwise have to reconstruct the arithmetic, and the arithmetic is
    /// where the mistakes are. Takes the target and the cursor rather than two
    /// byte counts, so that neither can be measured with the wrong function nor
    /// passed in the wrong order.
    let spliceWithinLimit (limits : PathLimits) (target : SymlinkTarget) (remaining : PathCursor) : bool =
        match limits.SpliceRecheck with
        | SpliceLengthRecheck.NoRecheck -> true
        | SpliceLengthRecheck.Recheck ->

        // Bytes throughout, never UTF-16 code units — measured with CJK, and
        // the distinction matters because `nameWithinLimit` next door
        // legitimately *does* count code units on Darwin.
        let targetBytes = UnixPathText.utf8.GetByteCount (SymlinkTarget.toString target)

        // The rule transcribes XNU's `linklen + ni_pathlen > MAXPATHLEN`, where
        // `linklen` is the target's raw byte length and `ni_pathlen` counts the
        // unconsumed remainder *including* the NUL — hence the `+ 1`, and hence
        // `<=` rather than `<`. Measured on Darwin 25.6.0: through a remainder
        // of "/a", a 1021-byte target resolves (1021 + 2 + 1 = 1024) and a
        // 1022-byte one does not.
        targetBytes + PathCursor.remainingBytes remaining + 1 <= limits.PathMaxBytes

    /// Re-check the invariant of a value that may not have come from `create`.
    ///
    /// Unlike `UnixTimestamp`, whose `Unchecked.defaultof` is the epoch and so
    /// perfectly legal, this type's default is a zero traversal limit — which
    /// `create` rejects, but which would otherwise make the *first* symlink on
    /// any path report ELOOP. That is the failure worth catching: not a crash,
    /// but a plausible-looking answer from a kernel that cannot exist, produced
    /// silently for every path in the filesystem.
    let assertValid (context : string) (limits : PathLimits) : PathLimits =
        // The integer checks come first, and the `NameMax` one is written to
        // avoid needing the unit: on a forged default every field is zero at
        // once, and this must report that rather than depend on which field
        // happens to be examined first.
        if limits.MaxSymlinkTraversals < 1 || limits.PathMaxBytes < 1 then
            failwith
                $"%s{context}: these path limits permit %d{limits.MaxSymlinkTraversals} symlink traversals and a PATH_MAX of %d{limits.PathMaxBytes} bytes, which no Unix does. A PathLimits that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; obtain one from SimulatedUnixPlatform.pathLimits instead."

        let nameMax =
            match limits.NameMax with
            | NameLengthLimit.Utf8Bytes bytes -> bytes
            | NameLengthLimit.Utf16CodeUnits units -> units

        if nameMax < 1 then
            failwith
                $"%s{context}: these path limits permit a NAME_MAX of %d{nameMax}, which would forbid every filename. A PathLimits that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; obtain one from SimulatedUnixPlatform.pathLimits instead."

        limits

/// What the bytes of a syscall's path argument name, once this kernel has
/// copied them in.
[<RequireQualifiedAccess>]
type PathArgument =
    | Parsed of path : UnixPath
    /// The entry point returns its failure sentinel, and the caller stores
    /// `error` wherever its libc keeps errno.
    ///
    /// This is what `getname()` reports, so it is `ENAMETOOLONG` — the only way
    /// a pathname's *bytes* can be wrong, everything else being a question about
    /// what they resolve to — or `EFAULT`, when the caller could not read the
    /// bytes at all. `PathArgument.parse` produces only the former, having been
    /// handed bytes already; a caller reading them out of a guest's memory
    /// produces the latter itself.
    ///
    /// Which one it is never changes where the failure surfaces. Measured on
    /// both kernels: with a source that does not exist, an unreadable
    /// destination pointer and an over-long destination path are reported at the
    /// same point in `rename(2)` — above the source's resolution on Linux, below
    /// it on Darwin — so a caller orders "the argument was copied in" as one
    /// step. See `RenameWalkOrder`.
    | Failed of error : UnixError

/// The bytes of a pathname argument as the caller found them, before anything
/// has decided what they say.
///
/// Distinct from `PathArgument`, which is what the bytes turned out to *mean*.
/// The two are separate because a syscall taking more than one pathname copies
/// them in at measured points and may never reach the second: `rename` on
/// Darwin resolves its source completely first, so a caller that decoded both up
/// front would refuse a pathname the kernel never looked at.
[<RequireQualifiedAccess>]
type PathArgumentBytes =
    /// The caller could not read the pathname at all, which is EFAULT wherever
    /// the kernel gets round to copying it in.
    | Unreadable
    /// The pathname's bytes **without a NUL terminator**, as `PathArgument.parse`
    /// takes them.
    | Bytes of bytes : ImmutableArray<byte>

/// Why this kernel cannot say what a path argument names.
///
/// A gap in *representation* rather than in measurement — what a real kernel
/// does is not in doubt — so a message composed for this should not claim to
/// report a measurement.
[<RequireQualifiedAccess>]
type PathArgumentRefusal =
    /// The bytes are not valid UTF-8. A real kernel looks up the raw bytes, so
    /// byte 0xFF names a file no valid UTF-8 name can; this kernel models a
    /// filename as a string of characters and has no such name to look up.
    | NotUtf8

[<RequireQualifiedAccess>]
module PathArgument =
    /// What a real kernel would look up, given the bytes of a path argument
    /// **without its NUL terminator** — which is what a caller that stopped at
    /// the NUL holds.
    ///
    /// The three stages run in this order, and the order is measured rather than
    /// arbitrary:
    ///
    ///  1. **Length first.** `PATH_MAX` is enforced by `getname()`/`copyinstr`
    ///     when the kernel copies the string in, before anything looks at what
    ///     it says. So a path that is *both* over-long and not valid UTF-8 is
    ///     `ENAMETOOLONG`, not a refusal — if the decode ran first, a path a real
    ///     kernel rejects cheaply would instead have no answer at all.
    ///  2. **Strict decode**, never a lenient one: substituting U+FFFD would
    ///     silently name a *different* file, one literally called "�".
    ///  3. **Parse.**
    ///
    /// The limit counts the NUL and these bytes do not, so the comparison is
    /// against `pathMaxBytes - 1`; and the limit is per-flavour (Darwin 1024,
    /// Linux 4096), which is why it arrives as `PathLimits` rather than as a
    /// constant.
    let parse (limits : PathLimits) (bytes : ImmutableArray<byte>) : Result<PathArgument, PathArgumentRefusal> =
        // A forged `PathLimits` has a `PathMaxBytes` of zero, under which *every*
        // path is over-long — a plausible-looking ENAMETOOLONG from a kernel that
        // cannot exist, produced silently for every path a guest names. Checked
        // before the limit is read, as `resolveFull` checks it before the walk.
        let limits = PathLimits.assertValid "PathArgument.parse" limits

        // `ImmutableArray` is a struct wrapping an array, so `default` carries a
        // null one and would throw on the `Length` read below rather than at the
        // point the mistake was made. Rejected rather than treated as empty: an
        // empty path argument is a real thing a guest passes, and `open("")` is
        // ENOENT, so silently conflating the two would answer about a path the
        // caller never had.
        if bytes.IsDefault then
            failwith
                "PathArgument.parse: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty path; pass ImmutableArray<byte>.Empty."

        if bytes.Length > PathLimits.pathMaxBytes limits - 1 then
            Ok (PathArgument.Failed UnixError.ENAMETOOLONG)
        else

        let decoded =
            try
                Some (UnixPathText.utf8.GetString (bytes.AsSpan ()))
            with :? System.Text.DecoderFallbackException ->
                None

        match decoded with
        | None -> Error PathArgumentRefusal.NotUtf8
        | Some decoded ->

        match UnixPath.parse decoded with
        | Ok path -> Ok (PathArgument.Parsed path)
        | Error error ->
            // Unreachable: the only rejections are a null candidate — impossible,
            // we have just decoded a string — and text that cannot survive the
            // `char*` boundary, which a string decoded *from* that boundary
            // cannot contain.
            failwith
                $"PathArgument.parse: the path did not survive parsing: %s{UnixPath.describe error}. The value was decoded from a NUL-free byte string, so it can contain neither an embedded NUL nor a null reference (this is a bug in this library)."
