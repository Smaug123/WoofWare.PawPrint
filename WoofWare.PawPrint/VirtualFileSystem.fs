namespace WoofWare.PawPrint

open System.Collections.Immutable

/// Identity of a file within the emulated filesystem: the `st_ino` a guest
/// reads back from `stat`.
///
/// Guest-observable, and not merely as a diagnostic: the BCL compares
/// `(Dev, Ino)` pairs directly to decide whether two paths name the same file
/// (`File.Copy`, `File.Move` and `File.Replace` all do this before touching
/// anything), so inode identity is part of the model rather than a refinement
/// of it.
[<Struct>]
type InodeNumber =
    | InodeNumber of value : int64

    override this.ToString () : string =
        match this with
        | InodeNumber value -> string<int64> value

/// Why a string is not usable as the target of a symbolic link.
[<RequireQualifiedAccess>]
type SymlinkTargetError =
    /// The candidate was null or empty. `symlink(2)` on Linux rejects an empty
    /// target with ENOENT — but macOS *accepts* it, creating a link that then
    /// fails to resolve. PawPrint refuses to represent one at all rather than
    /// picking a platform: with no such value in the model, the divergence can
    /// only arise at the `symlink` boundary, where it is a guest call that can
    /// be failed loudly, and never inside a seed manifest.
    | Empty
    /// The candidate could not survive the `char*` boundary; see
    /// `UnixPathTextDefect`.
    | Text of defect : UnixPathTextDefect

/// The target of a symbolic link, held exactly as it was created.
///
/// Verbatim rather than parsed, which matters: `readlink(2)` returns the stored
/// bytes unchanged, and `lstat` reports their length as the link's `st_size`, so
/// a link created with target "a//b/" must read back as "a//b/" — a difference a
/// guest really can see, through `FileInfo.LinkTarget` and `ResolveLinkTarget`.
///
/// The path structure is recovered by parsing at traversal time, which is
/// cheap, total, and keeps the stored form authoritative. `UnixPath` is kept
/// the same way and for a related reason (a kernel measures the bytes it was
/// handed), so converting a target to one loses nothing.
[<Struct>]
type SymlinkTarget =
    private
    | SymlinkTarget of target : string

    override this.ToString () : string =
        match this with
        | SymlinkTarget target -> target

[<RequireQualifiedAccess>]
module SymlinkTarget =
    let toString (target : SymlinkTarget) : string =
        match target with
        | SymlinkTarget target -> target

    /// Parse a symlink target. Total: never throws, for any input including
    /// null.
    let parse (candidate : string) : Result<SymlinkTarget, SymlinkTargetError> =
        if System.String.IsNullOrEmpty candidate then
            Error SymlinkTargetError.Empty
        else

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error (SymlinkTargetError.Text defect)
        | None -> Ok (SymlinkTarget candidate)

    let describe (error : SymlinkTargetError) : string =
        match error with
        | SymlinkTargetError.Empty ->
            "symlink target is null or empty; Linux rejects that with ENOENT while macOS accepts it, so PawPrint declines to represent it"
        | SymlinkTargetError.Text defect -> $"symlink target %s{UnixPathText.describe defect}"

    let parseOrFail (context : string) (candidate : string) : SymlinkTarget =
        match parse candidate with
        | Ok target -> target
        | Error error -> failwith $"%s{context}: %s{describe error} (got %s{candidate})"

    /// Re-check the invariant of a value that may not have come from `parse`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null payload would otherwise
    /// be stored as a symlink target that `checkInvariants` calls sound and
    /// that crashes only later, when some unrelated resolution happens to
    /// traverse it.
    let assertValid (context : string) (target : SymlinkTarget) : SymlinkTarget =
        match target with
        | SymlinkTarget raw ->

        match parse raw with
        | Ok _ -> target
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SymlinkTarget that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SymlinkTarget.parse instead."

    /// The path structure of the target, for a resolution walk to splice in.
    /// Total: `parse` has already discharged every rule `UnixPath.parse`
    /// enforces.
    let toUnixPath (target : SymlinkTarget) : UnixPath =
        let raw = toString target

        match UnixPath.parse raw with
        | Ok path -> path
        | Error error ->
            failwith
                $"SymlinkTarget.toUnixPath: %s{UnixPath.describe error} (got %s{raw}). Every SymlinkTarget satisfies UnixPath's invariant, so this cannot have come from SymlinkTarget.parse."

    /// The bytes `readlink(2)` hands back, and whose length is the link's
    /// `st_size`. Without a terminator: `readlink` does not write one.
    let toUtf8 (target : SymlinkTarget) : ImmutableArray<byte> =
        toString target |> UnixPathText.utf8.GetBytes |> ImmutableArray.CreateRange

/// The permission, set-user-ID, set-group-ID and sticky bits of an inode's
/// mode: `st_mode & 0o7777`, which is exactly `chmod(2)`'s domain.
///
/// Deliberately *not* the `S_IFMT` file-type band, which is derived from
/// `InodeContent` by `VirtualFileSystem.fileTypeBits` instead. That split is
/// not tidiness: `chmod(2)` cannot set the type band either, so keeping it out
/// of the stored value makes "the recorded type disagrees with the content"
/// unrepresentable rather than merely checked.
[<Struct>]
type PermissionBits =
    private
    | PermissionBits of bits : int

    override this.ToString () : string =
        match this with
        | PermissionBits bits -> "0o" + System.Convert.ToString(bits, 8).PadLeft (4, '0')

[<RequireQualifiedAccess>]
module PermissionBits =
    /// The widest `st_mode & 0o7777` can be: three rwx triples, plus setuid,
    /// setgid and the sticky bit.
    let private widest : int = 0o7777

    let toInt (bits : PermissionBits) : int =
        match bits with
        | PermissionBits bits -> bits

    /// Parse a raw mode word's permission bits, or `None` if it does not fit in
    /// `0o7777`.
    ///
    /// An `option` rather than this module's usual `Result` + `describe` pair,
    /// because there is exactly one way to fail and the offending value is the
    /// caller's own input, so a single-case error DU would carry no information
    /// the caller does not already hold. Note what this rejects: a caller
    /// passing a whole `st_mode` (type band included) is committing precisely
    /// the conflation this type exists to prevent, and is refused rather than
    /// silently masked down.
    let parse (candidate : int) : PermissionBits option =
        if candidate < 0 || candidate > widest then
            None
        else
            Some (PermissionBits candidate)

    let parseOrFail (context : string) (candidate : int) : PermissionBits =
        match parse candidate with
        | Some bits -> bits
        | None ->
            failwith
                $"%s{context}: 0o%s{System.Convert.ToString (candidate, 8)} is not a permission word; it must lie in [0, 0o7777]. If this is a whole st_mode, mask off the S_IFMT band — the file type is derived from InodeContent, never stored."

    /// What a `umask 022` process gets from `open(2)` with the 0o666 that
    /// CoreLib's `FileStream` passes: `0o666 &&& ~~~0o022`.
    ///
    /// Derived rather than invented, which is the point of writing it this way.
    /// PawPrint models no umask yet — nothing can read or set one (CoreLib's
    /// interop surface has no `SystemNative_UMask` at all) and no creating
    /// native exists — so a `Umask` field today would have no consumer that
    /// could make two inodes differ. It becomes correct at the first
    /// `open(O_CREAT)`/`mkdir`, and this constant becomes its consequence.
    let defaultForRegularFile : PermissionBits = PermissionBits (0o666 &&& ~~~0o022)

    /// What a `umask 022` process gets from `mkdir(2)`'s 0o777:
    /// `0o777 &&& ~~~0o022`. See `defaultForRegularFile`.
    let defaultForDirectory : PermissionBits = PermissionBits (0o777 &&& ~~~0o022)

/// A filesystem timestamp: `struct timespec`, whole seconds since the Unix
/// epoch plus a nanosecond part in `[0, 1e9)`.
///
/// Two fields rather than one nanosecond count. `st_atim.tv_sec` is a 64-bit
/// *second* count, so folding the pair into nanoseconds would cap the
/// representable range at 1677–2262 — and `File.SetLastWriteTime` will happily
/// be handed a `DateTime` outside it, which would then have to overflow or be
/// clamped. Neither is a thing a filesystem does.
///
/// Negative seconds are permitted: a pre-1970 mtime is ordinary, and `tar`
/// archives are full of them. A negative *nanosecond* part is not, matching the
/// kernel's own normalisation, so the pair always compares in the obvious
/// lexicographic order.
///
/// Note there is no `assertValid` counterpart to `FileName`'s: this type's
/// `Unchecked.defaultof` is `(0L, 0)`, the Unix epoch, which is a perfectly
/// legal timestamp. There is no forged value to catch.
[<Struct>]
type UnixTimestamp =
    private
    | UnixTimestamp of seconds : int64 * nanoseconds : int

    override this.ToString () : string =
        match this with
        | UnixTimestamp (seconds, nanoseconds) ->

        // A timespec is seconds *plus* nanoseconds, and the nanosecond part is
        // never negative — so a pre-epoch instant is not the two fields printed
        // adjacently with a minus in front. `(-1, 500_000_000)` is half a second
        // *before* the epoch; writing it "-1.500000000" would name a moment a
        // second earlier than the one it holds.
        if seconds >= 0L || nanoseconds = 0 then
            $"%d{seconds}.%09d{nanoseconds}"
        else
            // Carry the fraction the other way: s + n/1e9 = (s+1) - (1e9-n)/1e9.
            let whole = seconds + 1L
            let fraction = 1_000_000_000 - nanoseconds

            // `whole` of zero has lost the sign, since "0" and "-0" are the same
            // integer but only one of them is the right side of the epoch.
            if whole = 0L then
                $"-0.%09d{fraction}"
            else
                $"%d{whole}.%09d{fraction}"

[<RequireQualifiedAccess>]
module UnixTimestamp =
    let private nanosecondsPerSecond : int = 1_000_000_000

    let seconds (timestamp : UnixTimestamp) : int64 =
        match timestamp with
        | UnixTimestamp (seconds, _) -> seconds

    let nanoseconds (timestamp : UnixTimestamp) : int =
        match timestamp with
        | UnixTimestamp (_, nanoseconds) -> nanoseconds

    /// A timestamp, or `None` if the nanosecond part is not in `[0, 1e9)`.
    /// Deliberately not normalising an out-of-range part by carrying into the
    /// seconds: a caller who computed 1.5e9 nanoseconds has a unit bug, and
    /// silently absorbing it would hide it.
    let create (seconds : int64) (nanoseconds : int) : UnixTimestamp option =
        if nanoseconds < 0 || nanoseconds >= nanosecondsPerSecond then
            None
        else
            Some (UnixTimestamp (seconds, nanoseconds))

    let createOrFail (context : string) (seconds : int64) (nanoseconds : int) : UnixTimestamp =
        match create seconds nanoseconds with
        | Some timestamp -> timestamp
        | None ->
            failwith
                $"%s{context}: %d{nanoseconds} is not a nanosecond part; it must lie in [0, %d{nanosecondsPerSecond}). A whole-second count belongs in the seconds field."

    let ofSeconds (seconds : int64) : UnixTimestamp = UnixTimestamp (seconds, 0)

    /// A timestamp from a count of milliseconds since the Unix epoch, which is
    /// how the emulated kernel holds its wall clock.
    ///
    /// Floor division, so that a negative millisecond count keeps the
    /// nanosecond part non-negative rather than producing a `timespec` no
    /// kernel would write: -1 ms is (-1 s, 999 000 000 ns), not (0 s, -1e6 ns).
    ///
    /// Derived from the truncating quotient and remainder rather than by
    /// biasing the dividend. `(milliseconds - 999L) / 1000L` is the obvious way
    /// to floor a negative, and it silently overflows for the bottom 999 values
    /// of `int64`: it does not throw, it hands back a *positive* second count
    /// and a nanosecond part outside `[0, 1e9)` — a value that breaks the very
    /// invariant `create` exists to enforce, while bypassing it. Neither `/`
    /// nor `%` can overflow for any input here.
    let ofMillisecondsSinceEpoch (milliseconds : int64) : UnixTimestamp =
        let quotient = milliseconds / 1000L
        let remainder = milliseconds % 1000L

        if remainder >= 0L then
            UnixTimestamp (quotient, int remainder * 1_000_000)
        else
            // The truncating quotient rounded towards zero, so it names a
            // second later than the instant; the remainder is negative by
            // exactly the difference.
            UnixTimestamp (quotient - 1L, int (remainder + 1000L) * 1_000_000)

    /// The Unix epoch itself, which is also what a kernel booted at the default
    /// `WallClockEpochMs` of 0 believes the time to be.
    let epoch : UnixTimestamp = UnixTimestamp (0L, 0)

/// The four times a kernel keeps for an inode.
///
/// All four are stored on every platform, including `Birth` — which Linux's
/// `stat` does not report, but which *exists*: `pal_io.c` hard-zeroes it under
/// `#else` with the comment "Linux path: until we use statx()", so the fact is
/// real and merely unfetched. Modelling it here and gating only its *reporting*
/// on the simulated platform keeps the graph honest and confines the platform
/// flavour to the `stat` boundary where it belongs.
type InodeTimes =
    {
        /// `st_atim`: last read.
        Access : UnixTimestamp
        /// `st_mtim`: last change to the *contents*.
        Modification : UnixTimestamp
        /// `st_ctim`: last change to the *inode* — which `chmod`, `link` and
        /// `rename` all move even though they touch no content, and which is
        /// why this is stored rather than derived from `Modification`.
        StatusChange : UnixTimestamp
        /// `st_birthtim`: when the inode was created. Never moves afterwards.
        Birth : UnixTimestamp
    }

[<RequireQualifiedAccess>]
module InodeTimes =
    /// The times a freshly-created inode has: all four equal, because creation
    /// is simultaneously its birth, its last content change, its last inode
    /// change, and (vacuously) its last access.
    let createdAt (now : UnixTimestamp) : InodeTimes =
        {
            Access = now
            Modification = now
            StatusChange = now
            Birth = now
        }

    /// Record a change to the inode's contents: `mtime` and `ctime` both move,
    /// because changing what a file or directory holds also changes the inode
    /// that describes it. `atime` and `birth` do not.
    let contentsChangedAt (now : UnixTimestamp) (times : InodeTimes) : InodeTimes =
        { times with
            Modification = now
            StatusChange = now
        }

/// The contents of a directory: what it holds, and what contains it.
///
/// `Entries` holds only *real* names. "." and ".." are genuine directory
/// entries in a kernel, but storing them here would mean every traversal either
/// special-cased them or recursed forever, and would let the graph express a
/// directory whose "." pointed somewhere else. Both are instead derived — ".."
/// from `Parent`, "." from the directory itself — so they cannot disagree with
/// the structure they describe. `readdir` synthesises them.
type DirectoryContent =
    {
        Entries : Map<FileName, InodeNumber>
        /// The directory that holds this one, which is what ".." resolves to.
        /// The root is its own parent, exactly as on a real Unix.
        ///
        /// This is the *physical* parent, so it is still correct after a walk
        /// has crossed a symlink — the lexical predecessor in the path is not.
        Parent : InodeNumber
        /// The `chmod`-able bits of this directory's mode.
        Permissions : PermissionBits
    }

/// What lives at an inode. The `S_IFMT` file-type bits a guest reads from
/// `stat` are *derived* from which case this is, never stored, so the two can
/// never disagree.
///
/// Carries the metadata whose *existence* depends on which kind of thing this
/// is, and only that. A regular file and a directory have `chmod`-able
/// permission bits; a symbolic link does not, and the field is absent rather
/// than present-and-ignored — see `InodePermissions.PlatformSymlinkDefault` for
/// why a stored one could only ever describe a filesystem no kernel could
/// produce. Metadata that every inode has regardless (the four timestamps)
/// lives on `Inode` instead.
///
/// Names are compared with F#'s ordinal string comparison, so the emulated
/// filesystem is case-sensitive and normalisation-preserving. That is not a
/// platform divergence to crash on: case-sensitivity is a property of a
/// *filesystem* rather than of an OS (Linux mounts case-insensitive
/// directories; macOS runs case-sensitive APFS). It does mean the model
/// resembles a Linux default rather than a macOS one.
[<RequireQualifiedAccess>]
type InodeContent =
    | RegularFile of contents : ImmutableArray<byte> * permissions : PermissionBits
    | Directory of directory : DirectoryContent
    /// The link's target, unresolved: a symlink's target is a *string* to the
    /// kernel, re-resolved on every traversal, not a reference to whatever it
    /// pointed at when it was made.
    | Symlink of target : SymlinkTarget

/// One inode: what lives there, and the metadata every inode carries whatever
/// kind of thing it is.
type Inode =
    {
        Content : InodeContent
        Times : InodeTimes
    }

/// An inode's permission bits as a caller must handle them, which is not always
/// "here is a number".
///
/// A DU rather than an `option`, so that a caller cannot reach for a default
/// and quietly get the wrong answer: the symlink case is not "no permissions",
/// it is "the answer is a property of the platform, which this module cannot
/// see". `SimulatedUnixPlatform` lives in `EmulatedKernel.fs`, which compiles
/// after this file, and that layering is right rather than merely forced — the
/// graph is the model, and platform-flavoured presentation is a `stat`
/// concern.
[<RequireQualifiedAccess>]
type InodePermissions =
    /// A regular file's or directory's stored, `chmod`-able bits.
    | Stored of bits : PermissionBits
    /// A symbolic link's bits, which are **not stored** because no syscall
    /// PawPrint models can make two links differ: Linux has no `lchmod`,
    /// `chmod(2)` follows the link, and `fchmodat(AT_SYMLINK_NOFOLLOW)` is
    /// ENOTSUP there. Under Linux the answer is invariably 0o777, so a stored
    /// field could only ever express a filesystem no kernel could have
    /// produced.
    ///
    /// Platform-dependent, and measured rather than assumed: macOS applies the
    /// creating process's **umask** to a symlink (probed on this box: `umask
    /// 022` gives 0o755, `umask 077` gives 0o700, `umask 000` gives 0o777),
    /// while Linux reports 0o777 whatever the umask. So the caller — which
    /// knows the simulated platform — supplies the value.
    | PlatformSymlinkDefault

/// A whole emulated filesystem: an inode graph rooted at a single directory.
type VirtualFileSystem =
    private
        {
            Inodes : Map<InodeNumber, Inode>
            /// The directory absolute paths resolve from. Its `Parent` is
            /// itself.
            Root : InodeNumber
            /// The next inode number to hand out. Numbers are never reused,
            /// even after the last link to a file is removed: reuse is
            /// observable to a guest that cached an `(st_dev, st_ino)` pair,
            /// and a fresh number can only ever make a stale comparison report
            /// "different file", which is the safe direction to be wrong in.
            NextInode : InodeNumber
        }

/// A way in which a `VirtualFileSystem` fails to describe a filesystem any
/// kernel could produce. `VirtualFileSystem.checkInvariants` returns these;
/// none of the operations in this module can produce one.
[<RequireQualifiedAccess>]
type VirtualFileSystemDefect =
    /// `Root` names an inode the graph does not contain.
    | RootMissing of root : InodeNumber
    /// `Root` names something other than a directory.
    | RootIsNotDirectory of root : InodeNumber
    /// The root's `Parent` is not the root. On a real Unix "/.." is "/".
    | RootParentIsNotSelf of root : InodeNumber * recordedParent : InodeNumber
    /// Some directory holds an entry pointing at the root. The root is the one
    /// directory with *no* incoming entry link; giving it one would make the
    /// graph cyclic while leaving every individual link count plausible.
    | RootHasIncomingLink of parents : (InodeNumber * FileName) list
    /// A directory entry points at an inode the graph does not contain.
    | DanglingEntry of directory : InodeNumber * name : FileName * target : InodeNumber
    /// A directory's `Parent` names an inode the graph does not contain.
    | DanglingParent of directory : InodeNumber * recordedParent : InodeNumber
    /// A directory's `Parent` names something that is not a directory.
    | ParentIsNotDirectory of directory : InodeNumber * recordedParent : InodeNumber
    /// A directory's `Parent` disagrees with the directory that actually holds
    /// it, so ".." would walk somewhere the path did not come from.
    | ParentMismatch of directory : InodeNumber * recordedParent : InodeNumber * actualParent : InodeNumber
    /// A directory is held by more than one entry. Unix forbids hard links to
    /// directories precisely because they would make the graph a non-tree, and
    /// `Parent` could then name only one of them.
    | DirectoryMultiplyLinked of directory : InodeNumber * parents : (InodeNumber * FileName) list
    /// An inode no path from the root can reach. Every inode is reachable in a
    /// real filesystem unless a process holds it open after its last link went
    /// away — which this model cannot yet express, because it has no open file
    /// descriptions. This defect relaxes when it grows them.
    | UnreachableFromRoot of inode : InodeNumber
    /// `NextInode` would hand out a number already in use.
    | NextInodeNotFresh of nextInode : InodeNumber * existing : InodeNumber

/// Whether a resolution follows a symlink found in the *final* position.
/// Symlinks in every earlier position are always followed; a path cannot
/// continue through one otherwise.
///
/// A trailing separator overrides this — see `Resolution`.
[<RequireQualifiedAccess>]
type SymlinkPolicy =
    /// Follow it: what `stat`, and `open` without `O_NOFOLLOW`, do.
    | Follow
    /// Stop at the link itself: what `lstat`, `readlink`, `unlink`, `rename`
    /// and `open` with `O_NOFOLLOW` do.
    | NoFollowFinal

/// Which component a resolution last consumed, for the paths that end without
/// a name to look up.
///
/// Carried on `ResolvedTarget.Directory` rather than left to the caller to read
/// off its own path, because a symlink expansion replaces the final component:
/// with `l1 -> "."` and `l2 -> "d/.."`, the paths "l1/" and "l2/" are the same
/// shape but land on different navigation. Probed on macOS, `rmdir` owes them
/// different errnos — EINVAL and ENOTEMPTY respectively — so the distinction is
/// guest-observable and unrecoverable from the original path.
[<RequireQualifiedAccess>]
type FinalNavigation =
    /// The path named no component at all: "/" itself, or a symlink whose
    /// target was "/". `rmdir` owes this EBUSY on Linux.
    | Root
    /// The last component consumed was ".". `rmdir` owes this EINVAL.
    | Current
    /// The last component consumed was "..". `rmdir` owes this ENOTEMPTY.
    | Parent

/// Where a path resolution ended up.
[<RequireQualifiedAccess>]
type ResolvedTarget =
    /// The path named `Name` inside `Directory`. `Existing` is the inode bound
    /// to that name, or `None` when the name is free — which is *not* an error
    /// here, because it is exactly the state `open(O_CREAT)`, `mkdir` and
    /// `symlink` need, and callers that require the file to exist report
    /// ENOENT themselves.
    | Entry of directory : InodeNumber * name : FileName * existing : InodeNumber option
    /// The path resolved straight to a directory with no final name to look
    /// up, because its last component — after any symlink expansion — was "/",
    /// "." or "..".
    ///
    /// `ReachedBy` says which, because the errno that follows depends on it and
    /// the caller cannot recover it from the path it passed in: see
    /// `FinalNavigation`. (Note ENOTEMPTY is itself platform-dependent — Linux
    /// 39, Darwin 66 — so it will join `UnixError` the way `ELOOP` did.)
    | Directory of inode : InodeNumber * reachedBy : FinalNavigation

/// The outcome of a resolution, together with the facts about *how* it
/// finished that a caller cannot recover from the path it passed in.
type Resolution =
    {
        Target : ResolvedTarget
        /// The path, after any final symlink was spliced in, ended with a
        /// separator — so it demanded that its final component be a directory.
        ///
        /// Not simply the caller's `UnixPath.hasTrailingSeparator`: following a
        /// final symlink replaces the final path segment, so a link whose
        /// target is "d/" imposes the demand even when the guest's own path did
        /// not. The unanimous part of the rule is enforced here (an *existing*
        /// non-directory final gives ENOTDIR). The part that is not unanimous
        /// is left to the caller — see `FinalSymlinkFollowed`.
        TrailingSeparatorDemanded : bool
        /// A symlink in the *final* position was followed to get here.
        ///
        /// Load-bearing in combination with `TrailingSeparatorDemanded`,
        /// because that pair is where Linux and macOS genuinely disagree, and
        /// disagree destructively. Probed on macOS: with `ld -> realdir`,
        /// `rmdir("ld/")` *removes realdir*; with `dang -> nx`, `mkdir("dang/")`
        /// *creates nx*. Linux refuses both (ENOTDIR, EEXIST). A mutating
        /// operation that sees both flags set must therefore fail loudly rather
        /// than pick a platform, since the two choices destroy different
        /// objects. Lookup operations (`stat`, `lstat`) are unanimous and can
        /// ignore this.
        FinalSymlinkFollowed : bool
    }

/// The bounds a kernel puts on path resolution, which differ between the Unixes
/// PawPrint models and so cannot be constants in the walk.
///
/// Deliberately not `SimulatedUnixPlatform` itself, which lives in
/// `EmulatedKernel` and compiles later — the same split `RawErrnoNumbering`
/// makes, and for the same reason: this file has no business knowing what a
/// platform *is*, only that something has chosen limits.
/// `SimulatedUnixPlatform.pathLimits` is the mapping, and is where the numbers
/// are justified as measured facts about real kernels.
///
/// Deliberately *not* a field of `VirtualFileSystem` either, which would have
/// saved threading it through every call. A `VirtualFileSystem` is a filesystem
/// *image*: it comes from a seed, which has no platform and could not sensibly
/// acquire one. `MAXSYMLINKS` is a property of the kernel doing the walking, not
/// of the tree being walked, and storing it in the image would let two
/// filesystems under one kernel disagree about it — a state no real system can
/// be in.
///
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
    /// ext4, and Linux filesystems generally: a raw byte count, which is what
    /// the kernel stores and compares.
    | Utf8Bytes of bytes : int
    /// APFS (and HFS+ before it), which stores names as UTF-16 and bounds the
    /// count of code units. Conveniently exactly `String.Length`, because .NET
    /// strings are UTF-16 — a coincidence worth naming, since it makes the
    /// *wrong* implementation look right on a Mac.
    | Utf16CodeUnits of units : int

/// Whether expanding a symbolic link re-checks that the path still fits in
/// `PATH_MAX`.
///
/// Measured, by bisecting the symlink-target length at which a dangling link
/// flips ENOENT → ENAMETOOLONG (Darwin 25.6.0 / macOS 26.6 and Linux 6.18.5).
/// Darwin refuses when `linklen + ni_pathlen > MAXPATHLEN` — XNU's `lookup`
/// splices by copying the target and the unconsumed remainder into a fresh
/// `MAXPATHLEN` buffer, so the rule is simply that the new buffer must fit.
/// Linux has no such check *at all*: measured, a 3842-byte target with an
/// 806-byte remainder resolves at 4648 bytes spliced, well past its own
/// `PATH_MAX`.
///
/// So this is not a difference of degree that a number could express. One
/// kernel performs a check the other does not perform at any threshold, which
/// is why it is a DU and not, say, a nullable limit.
///
/// A struct for the reason `NameLengthLimit` gives. Its forged default is
/// `Recheck`, and that ordering is deliberate: `PathLimits.assertValid` is what
/// actually rejects a forged value (via the integer fields, which are zero),
/// but were that guard ever weakened, a spurious ENAMETOOLONG is a visible
/// wrong answer where a silently skipped check is an invisible one.
[<RequireQualifiedAccess>]
[<Struct>]
type SpliceLengthRecheck =
    /// Darwin. The spliced path — target bytes, unconsumed remainder, and the
    /// NUL — must still fit in `PATH_MAX`.
    | Recheck
    /// Linux. A path may grow without bound as links are expanded, so long as
    /// each *component* is within `NAME_MAX` and the original argument was
    /// within `PATH_MAX`.
    | NoRecheck

/// A record rather than a bare `int`: `MAXSYMLINKS`, `PATH_MAX` and `NAME_MAX`
/// are the same kind of fact, and a caller that needs one generally needs the
/// others.
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
    /// look correct, which is precisely the bug this shape prevents.
    let nameWithinLimit (limits : PathLimits) (name : FileName) : bool =
        match limits.NameMax with
        | NameLengthLimit.Utf8Bytes bytes -> UnixPathText.utf8.GetByteCount (FileName.toString name) <= bytes
        | NameLengthLimit.Utf16CodeUnits units -> (FileName.toString name).Length <= units

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
    ///
    /// The rule transcribes XNU's `linklen + ni_pathlen > MAXPATHLEN`, where
    /// `linklen` is the target's raw byte length and `ni_pathlen` counts the
    /// unconsumed remainder *including* the NUL — hence the `+ 1`, and hence
    /// `<=` rather than `<`. Measured on Darwin 25.6.0: through a remainder of
    /// "/a", a 1021-byte target resolves (1021 + 2 + 1 = 1024) and a 1022-byte
    /// one does not.
    ///
    /// Bytes throughout, never UTF-16 code units — measured with CJK, and the
    /// distinction matters because `nameWithinLimit` next door legitimately
    /// *does* count code units on Darwin.
    let spliceWithinLimit (limits : PathLimits) (target : SymlinkTarget) (remaining : PathCursor) : bool =
        match limits.SpliceRecheck with
        | SpliceLengthRecheck.NoRecheck -> true
        | SpliceLengthRecheck.Recheck ->

        let targetBytes = UnixPathText.utf8.GetByteCount (SymlinkTarget.toString target)

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

/// Where `lseek(2)` measures its offset from.
///
/// Exactly the three `Interop.Sys.SeekWhence` values (`Interop.LSeek.cs`), which
/// are also the three POSIX ones — and *not* the platforms' full `<unistd.h>`
/// vocabulary, which continues with `SEEK_DATA` and `SEEK_HOLE`. Those two are
/// deliberately absent: they are numbered 3 and 4 on Linux and **4 and 3** on
/// Darwin, so a raw whence of 3 does not name the same operation on the two
/// kernels, and there is no portable case to add. `SystemNative_LSeek` decodes
/// the raw integer and refuses them; see the handler.
[<RequireQualifiedAccess>]
type SeekWhence =
    /// `SEEK_SET` (0): from the start of the file.
    | Set
    /// `SEEK_CUR` (1): from the description's current offset.
    | Current
    /// `SEEK_END` (2): from the end of the file.
    | End

/// Why a seek computation has no answer.
///
/// Split into two cases rather than one because the platforms disagree about
/// only one of them: a computation landing below zero is `EINVAL` on both,
/// while one that leaves `int64` is `EINVAL` on Linux and `EOVERFLOW` on Darwin.
/// Collapsing them here would push that distinction into the handler as a
/// second computation of the same arithmetic.
[<RequireQualifiedAccess>]
type SeekFault =
    /// The computed position is negative. Real kernels reject rather than
    /// clamp, so a file offset is never pinned to 0 by a wild seek.
    | Negative
    /// The computed position does not fit in a signed 64-bit offset. Only
    /// reachable through `SEEK_CUR` and `SEEK_END`, whose arithmetic adds two
    /// values the caller does not jointly control.
    | Overflow

[<RequireQualifiedAccess>]
module VirtualFileSystem =
    /// Inode 1, matching the convention that no real filesystem hands out inode
    /// 0. A zero default would otherwise silently alias whichever inode was
    /// allocated first.
    let private firstInode : InodeNumber = InodeNumber 1L

    /// The `S_IFMT` band of `st_mode`: which kind of thing lives at an inode.
    /// Derived from the content rather than stored, so the two cannot disagree.
    ///
    /// The values are `Interop.Sys.FileTypes`' (`Interop.Stat.cs`), which are in
    /// turn the POSIX ones. `TestVirtualFileSystemAgainstHost` pins them against
    /// that declaration *as read from the pinned runtime source*, rather than
    /// against a second copy of the same literals, so a typo here cannot survive
    /// as a plausible-looking lie.
    let fileTypeBits (content : InodeContent) : int =
        match content with
        | InodeContent.RegularFile _ -> 0o100000
        | InodeContent.Directory _ -> 0o40000
        | InodeContent.Symlink _ -> 0o120000

    /// An inode's permission bits, as something the caller must match rather
    /// than a number it might default. See `InodePermissions`.
    let permissions (inode : Inode) : InodePermissions =
        match inode.Content with
        | InodeContent.RegularFile (_, permissions) -> InodePermissions.Stored permissions
        | InodeContent.Directory directory -> InodePermissions.Stored directory.Permissions
        | InodeContent.Symlink _ -> InodePermissions.PlatformSymlinkDefault

    /// A filesystem containing nothing but an empty root directory, created at
    /// `now`.
    ///
    /// Takes the time rather than reading a clock: this file compiles before
    /// `EmulatedKernel.fs`, and more to the point a filesystem that read the
    /// host's clock would make a replay depend on when it was recorded.
    let empty (now : UnixTimestamp) : VirtualFileSystem =
        {
            Inodes =
                Map.ofList
                    [
                        firstInode,
                        {
                            Content =
                                InodeContent.Directory
                                    {
                                        Entries = Map.empty
                                        Parent = firstInode
                                        Permissions = PermissionBits.defaultForDirectory
                                    }
                            Times = InodeTimes.createdAt now
                        }
                    ]
            Root = firstInode
            NextInode = InodeNumber 2L
        }

    let root (vfs : VirtualFileSystem) : InodeNumber = vfs.Root

    let nextInode (vfs : VirtualFileSystem) : InodeNumber = vfs.NextInode

    let inodes (vfs : VirtualFileSystem) : Map<InodeNumber, Inode> = vfs.Inodes

    let tryGet (inode : InodeNumber) (vfs : VirtualFileSystem) : Inode option = Map.tryFind inode vfs.Inodes

    /// What lives at `inode`, discarding its metadata. A projection, for the
    /// many callers that are asking a question about the *shape* of the graph;
    /// `tryGet` is the one that answers about identity.
    let tryGetContent (inode : InodeNumber) (vfs : VirtualFileSystem) : InodeContent option =
        Map.tryFind inode vfs.Inodes |> Option.map (fun inode -> inode.Content)

    /// How many bytes a read of `count` bytes starting at `offset` transfers,
    /// from a file whose contents are `length` bytes long.
    ///
    /// Separated out because it is the whole of what `pread(2)` decides beyond
    /// its error cases, and because getting it wrong is an off-by-one that
    /// end-to-end tests report as "the file came back slightly wrong" from
    /// somewhere deep in a `StreamReader`. As a function of three integers it is
    /// property-testable against naive slicing instead.
    ///
    /// Note the result is what a *regular file* transfers, which is why this can
    /// be total: a short read is only ever "the file ended". Real `read(2)` may
    /// return fewer bytes than asked for on a pipe or socket with nothing to do
    /// with EOF, and nothing here models that.
    ///
    /// Reading at or past the end is 0 rather than an error — measured, and the
    /// same on Linux and Darwin. So is a zero-length request, which is why
    /// callers must not treat 0 as EOF-specific.
    let readTransferCount (offset : int64) (count : int) (length : int) : int =
        // The handler is responsible for rejecting a negative offset (EINVAL)
        // and refusing a negative size, so both are established before here.
        System.Diagnostics.Debug.Assert (offset >= 0L, "readTransferCount: offset must not be negative")
        System.Diagnostics.Debug.Assert (count >= 0, "readTransferCount: count must not be negative")
        System.Diagnostics.Debug.Assert (length >= 0, "readTransferCount: length must not be negative")

        if offset >= int64 length then
            // Includes an offset beyond `int` range, which no seeded file can
            // reach but a guest can certainly ask for.
            0
        else
            // `length - offset` is in `(0, length]` here, so the `int` conversion
            // cannot overflow however large `offset` was.
            min (int64 count) (int64 length - offset) |> int

    /// Where `lseek(2)` would land, given where it is measuring from.
    ///
    /// The whole of what `lseek` computes, separated out for the same reason as
    /// `readTransferCount`: as a function of four integers it is
    /// property-testable, where the same arithmetic inlined in a handler is
    /// reachable only through a guest.
    ///
    /// **Not bounded above by `size`.** Seeking past the end of a file is legal
    /// — it is how sparse files are made — and a subsequent read there simply
    /// transfers nothing. The only rejections are the two `SeekFault` cases.
    ///
    /// **No filesystem ceiling either**, which is a measured decision rather than
    /// an omission. A real Linux rejects an offset above the filesystem's
    /// `s_maxbytes` with `EINVAL`: measured, ext4 stops at `0xffffffff000` while
    /// **tmpfs accepts the full `int64` range**, as does macOS's APFS. PawPrint's
    /// filesystem is in memory, so tmpfs is the honest analogue and the ceiling
    /// is `Int64.MaxValue`. Reading that divergence as a *platform* difference —
    /// which is how it first presents, since a dev box's APFS accepts what a CI
    /// container's ext4 refuses — would have written a false rule into the kernel
    /// model.
    /// **The size is deferred**, and that is load-bearing rather than an
    /// optimisation: only `SEEK_END` consults it, and there are descriptors with
    /// no size PawPrint is willing to state — a directory's, which is a
    /// filesystem artefact rather than a fact (see the `SystemNative_LSeek`
    /// handler). Seeking such a descriptor with `SEEK_SET` or `SEEK_CUR` is
    /// perfectly portable and must keep working, so the caller passes a thunk
    /// that refuses, and the machine rather than a comment enforces that only
    /// the `End` case forces it.
    let seekTarget
        (whence : SeekWhence)
        (current : int64)
        (size : Lazy<int64>)
        (offset : int64)
        : Result<int64, SeekFault>
        =
        // A property of the model rather than of the guest: a description's
        // offset is established non-negative by this very function.
        System.Diagnostics.Debug.Assert (current >= 0L, "seekTarget: the current offset must not be negative")

        let basis =
            match whence with
            | SeekWhence.Set -> 0L
            | SeekWhence.Current -> current
            | SeekWhence.End ->
                let size = size.Force ()

                System.Diagnostics.Debug.Assert (size >= 0L, "seekTarget: the file size must not be negative")

                size

        // Checked addition by inspection rather than by `Checked.(+)`, so that
        // overflow is a value this function returns rather than an exception
        // its caller must catch. `basis` is non-negative, so only a positive
        // `offset` can carry past `Int64.MaxValue`.
        if offset > 0L && basis > System.Int64.MaxValue - offset then
            Error SeekFault.Overflow
        else

        let target = basis + offset

        if target < 0L then Error SeekFault.Negative else Ok target

    /// The directory at `inode`, or `None` if it is absent or is not a
    /// directory. Honest about which: callers that must distinguish ENOENT from
    /// ENOTDIR use `tryGetContent` and match.
    let private tryGetDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : DirectoryContent option =
        match tryGetContent inode vfs with
        | Some (InodeContent.Directory directory) -> Some directory
        | Some _
        | None -> None

    let private allocate
        (content : InodeContent)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : InodeNumber * VirtualFileSystem
        =
        let inode = vfs.NextInode
        let (InodeNumber raw) = inode

        let vfs =
            { vfs with
                Inodes =
                    Map.add
                        inode
                        {
                            Content = content
                            Times = InodeTimes.createdAt now
                        }
                        vfs.Inodes
                NextInode = InodeNumber (raw + 1L)
            }

        inode, vfs

    /// Whether `name` could be bound in `directory` right now, with the errno
    /// the attempt would otherwise fail with.
    ///
    /// Separate from `bind` because the creators must check the *parent* before
    /// allocating the child. Allocating first is not merely wasteful: a
    /// `directory` that does not exist but happens to equal `NextInode` would
    /// be *created* by the allocation, so `bind` would then find it, bind the
    /// new inode as its own child, and return `Ok` for a filesystem unreachable
    /// from the root — instead of the ENOENT the operation promises.
    let private ensureBindable
        (directory : InodeNumber)
        (name : FileName)
        (vfs : VirtualFileSystem)
        : Result<unit, UnixError>
        =
        match tryGetContent directory vfs with
        | None -> Error UnixError.ENOENT
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->
            if
                Map.containsKey (FileName.assertValid "VirtualFileSystem: directory entry name" name) content.Entries
            then
                Error UnixError.EEXIST
            else
                Ok ()

    /// Bind `name` to `inode` in `directory`, which must exist, be a directory,
    /// and not already hold `name`.
    let private bind
        (directory : InodeNumber)
        (name : FileName)
        (inode : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        // Every builder binds through here, so this is the one place a name
        // enters the graph — and the one place a forged `default(FileName)` can
        // be stopped before it becomes an entry no path could ever name.
        let name = FileName.assertValid "VirtualFileSystem: directory entry name" name

        match Map.tryFind directory vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some ({
                    Content = InodeContent.RegularFile _
                })
        | Some ({
                    Content = InodeContent.Symlink _
                }) -> Error UnixError.ENOTDIR
        | Some ({
                    Content = InodeContent.Directory content
                } as existing) ->
            if Map.containsKey name content.Entries then
                Error UnixError.EEXIST
            else

            // Gaining an entry changes what the directory holds, so its `mtime`
            // moves, and with it the `ctime` of the inode describing it. Done
            // here because this is the single chokepoint through which a
            // directory ever gains an entry, so no builder can forget it.
            let updated =
                {
                    Content =
                        InodeContent.Directory
                            { content with
                                Entries = Map.add name inode content.Entries
                            }
                    Times = InodeTimes.contentsChangedAt now existing.Times
                }

            Ok
                { vfs with
                    Inodes = Map.add directory updated vfs.Inodes
                }

    /// Create an empty subdirectory. Mirrors `mkdir(2)`: EEXIST if the name is
    /// taken, ENOTDIR if `directory` is not a directory, ENOENT if it is absent.
    let createDirectory
        (directory : InodeNumber)
        (name : FileName)
        (permissions : PermissionBits)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate
                (InodeContent.Directory
                    {
                        Entries = Map.empty
                        Parent = directory
                        Permissions = permissions
                    })
                now
                vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a regular file with the given contents. Mirrors `open(2)` with
    /// `O_CREAT | O_EXCL`.
    let createFile
        (directory : InodeNumber)
        (name : FileName)
        (permissions : PermissionBits)
        (now : UnixTimestamp)
        (contents : ImmutableArray<byte>)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        // `ImmutableArray` is a struct wrapping an array, so `default` carries a
        // null one: it stores happily, passes `checkInvariants`, and throws only
        // when some later read touches `Length`. Rejected here for the same
        // reason as a forged `FileName` or `SymlinkTarget`, and deliberately
        // rejected rather than normalised to `Empty` — a caller who wrote
        // `default` meant something, and quietly turning it into an empty file
        // would hide the bug rather than surface it.
        if contents.IsDefault then
            failwith
                "VirtualFileSystem.createFile: contents is the default ImmutableArray, whose underlying array is null. That is not an empty file — it is an uninitialised value that would pass checkInvariants and then throw on the first read. Pass ImmutableArray<byte>.Empty for an empty file."

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated =
            allocate (InodeContent.RegularFile (contents, permissions)) now vfs

        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a symbolic link holding `target` verbatim. Mirrors `symlink(2)`,
    /// including that the target is not resolved, need not exist, and may be
    /// relative. An empty target is unrepresentable by construction; see
    /// `SymlinkTargetError.Empty` for why that is a refusal rather than an
    /// omission.
    /// Note there is no `permissions` parameter, and that is deliberate rather
    /// than an omission: see `InodePermissions.PlatformSymlinkDefault`.
    let createSymlink
        (directory : InodeNumber)
        (name : FileName)
        (now : UnixTimestamp)
        (target : SymlinkTarget)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        let target = SymlinkTarget.assertValid "VirtualFileSystem.createSymlink" target

        match ensureBindable directory name vfs with
        | Error error -> Error error
        | Ok () ->

        let inode, allocated = allocate (InodeContent.Symlink target) now vfs
        bind directory name inode now allocated |> Result.map (fun vfs -> inode, vfs)

    /// Bind an existing inode under a second name. Mirrors `link(2)`, including
    /// its refusal to hard-link a directory (EPERM): that would make the graph
    /// a non-tree, and a directory's `Parent` could then name only one of its
    /// containers.
    let hardLink
        (directory : InodeNumber)
        (name : FileName)
        (target : InodeNumber)
        (now : UnixTimestamp)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        match Map.tryFind target vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some {
                   Content = InodeContent.Directory _
               } -> Error UnixError.EPERM
        | Some ({
                    Content = InodeContent.RegularFile _
                } as existing)
        | Some ({
                    Content = InodeContent.Symlink _
                } as existing) ->
            match bind directory name target now vfs with
            | Error error -> Error error
            | Ok bound ->
                // The target's own `ctime` moves too: its link count changed,
                // which is a change to the inode even though its contents are
                // untouched. Its `mtime` does not. (`bind` has already moved the
                // *directory's* pair.)
                Ok
                    { bound with
                        Inodes =
                            Map.add
                                target
                                { existing with
                                    Times =
                                        { existing.Times with
                                            StatusChange = now
                                        }
                                }
                                bound.Inodes
                    }

    // ------------------------------------------------------------ resolution

    /// The directory ".." names from `directory`. The root is its own parent.
    /// Fails loudly rather than guessing when the graph does not say: a walk
    /// that reached `directory` has already established it is a directory, so
    /// a missing `Parent` is a broken graph rather than a guest error.
    let private parentOf (directory : InodeNumber) (vfs : VirtualFileSystem) : InodeNumber =
        match tryGetDirectory directory vfs with
        | Some content -> content.Parent
        | None ->
            failwith
                $"VirtualFileSystem: resolving \"..\" from inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

    /// Resolve `path` against `startDirectory`, which is where a *relative*
    /// path begins; a rooted path ignores it and starts at the root.
    ///
    /// The walk stops *short of* the final lookup rather than performing it, so
    /// one walk serves `stat`, `open`, `mkdir`, `unlink` and `rename` alike:
    /// see `ResolvedTarget`.
    ///
    /// A trailing separator is deliberately *not* desugared into a "." component
    /// here, even though POSIX describes the two as equivalent. They are not,
    /// for anything that mutates: probed on macOS, `mkdir("d/")` succeeds while
    /// `mkdir("nx/.")` gives ENOENT, and `rmdir("d/")` succeeds while
    /// `rmdir("d/.")` gives EINVAL. Desugaring would also collapse the
    /// `Entry` that `mkdir("d/")` needs into a `Directory`. The demand is
    /// instead recorded on `Resolution` and enforced only where every platform
    /// agrees.
    let resolveFull
        (limits : PathLimits)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
        // Checked here rather than trusted, because this is the boundary a
        // forged value crosses: `create` refuses a zero limit, but a struct's
        // `Unchecked.defaultof` carries one anyway.
        let limits = PathLimits.assertValid "VirtualFileSystem.resolveFull" limits

        // POSIX gives the empty path ENOENT (probed on both). Walking zero
        // components would instead silently answer "the directory I started
        // from".
        if UnixPath.isEmpty path then
            Error UnixError.ENOENT
        else

        let start =
            if UnixPath.isRooted path then
                Ok vfs.Root
            else

            match tryGetContent startDirectory vfs with
            | None -> Error UnixError.ENOENT
            | Some (InodeContent.Directory _) -> Ok startDirectory
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR

        match start with
        | Error error -> Error error
        | Ok start ->

        /// The walk returns the trailing-separator demand and whether it
        /// followed a final symlink alongside its outcome. `symlinks` counts the
        /// traversals so far, and is what `limits` bounds: it is a parameter
        /// rather than a returned fact because the only question anyone asks of
        /// it — has this kernel given up? — is answered here, in walk order.
        let rec walk
            (directory : InodeNumber)
            (remaining : PathCursor)
            (trailing : bool)
            (finalSymlinkFollowed : bool)
            (lastNavigation : FinalNavigation)
            (symlinks : int)
            : Result<Resolution, UnixError>
            =
            match PathCursor.next remaining with
            // Reached when the path has no name left to look up: after a "." or
            // "..", or immediately for a path that named no component at all.
            | None ->
                Ok
                    {
                        Target = ResolvedTarget.Directory (directory, lastNavigation)
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }
            | Some (PathComponent.Current, rest) ->
                walk directory rest trailing finalSymlinkFollowed FinalNavigation.Current symlinks
            | Some (PathComponent.Parent, rest) ->
                walk (parentOf directory vfs) rest trailing finalSymlinkFollowed FinalNavigation.Parent symlinks
            | Some (PathComponent.Name name, rest) ->

            // Before the lookup, and before anything notices whether the name
            // exists — which is what reproduces the measured precedence on both
            // kernels: "<300 bytes>/x" is ENAMETOOLONG (the over-long component
            // is reached and rejected) while "nxdir/<300 bytes>" is ENOENT (the
            // walk fails at the missing parent and never reaches it). Checking
            // after the lookup would report ENOENT for an over-long name that
            // does not exist, which is exactly what `stat` on a fresh long name
            // does *not* do.
            //
            // This is also the only place that sees components spliced in from a
            // symlink target, which a check at the syscall boundary could not.
            if not (PathLimits.nameWithinLimit limits name) then
                Error UnixError.ENAMETOOLONG
            else

            let entries =
                match tryGetDirectory directory vfs with
                | Some content -> content.Entries
                | None ->
                    failwith
                        $"VirtualFileSystem: looking up \"%s{FileName.toString name}\" in inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

            let isFinal = PathCursor.isExhausted rest

            let finish (target : ResolvedTarget) : Result<Resolution, UnixError> =
                Ok
                    {
                        Target = target
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    }

            match Map.tryFind name entries with
            | None ->
                if isFinal then
                    // Not an error: the caller decides whether a free name is
                    // ENOENT (`stat`) or the point of the call (`mkdir`). A
                    // trailing separator does not change that — `mkdir("nx/")`
                    // creates on both platforms.
                    finish (ResolvedTarget.Entry (directory, name, None))
                else
                    Error UnixError.ENOENT
            | Some target ->

            let content =
                match tryGetContent target vfs with
                | Some content -> content
                | None ->
                    failwith
                        $"VirtualFileSystem: directory inode %O{directory} binds \"%s{FileName.toString name}\" to inode %O{target}, which the graph does not contain. Run VirtualFileSystem.checkInvariants."

            // A trailing separator forces the final symlink to be followed even
            // under NoFollowFinal: POSIX resolves "p/" as "p/.", and both
            // platforms agree for lookups (probed: `lstat("ld/")` stats the
            // directory the link names). Where they *disagree* is what a
            // mutating caller may then do, which is why the fact is reported
            // rather than acted on.
            let followFinal = policy = SymlinkPolicy.Follow || trailing

            match content with
            | InodeContent.Symlink linkTarget when not isFinal || followFinal ->
                // Checked *before* the traversal happens, so a limit of 32 means
                // the 33rd attempt is the one that fails, matching both probes
                // (macOS resolves a chain of 32 and gives ELOOP at 33; Linux
                // resolves 40 and gives ELOOP at 41).
                //
                // This is also the bound that makes the walk terminate, which is
                // why there is no cycle detection: a seen-state set would *not*
                // be sufficient, because a link whose target names itself with a
                // suffix ("l" with target "l/x") grows the pathname buffer
                // forever without ever repeating a state. Only a count stops
                // that — and both real kernels use only a counter too, so this
                // is a transcription of their behaviour rather than an
                // approximation of it.
                if symlinks + 1 > PathLimits.maxSymlinkTraversals limits then
                    Error UnixError.ELOOP
                else if

                    // *After* the traversal count, because that is the order a
                    // kernel checks them in and the two disagree. Measured on
                    // Darwin: a chain whose last link both exhausts the budget and
                    // would overflow the length reports ELOOP, while the same chain
                    // one link shorter reports ENAMETOOLONG. XNU tests
                    // `ni_loopcnt` in `namei` before it ever reads the target.
                    //
                    // Before the splice rather than after, so an overflowing
                    // expansion is refused rather than performed — and note this
                    // sees `rest`, whose cursor already sits past the separator run
                    // the kernel collapsed, which is the whole reason the walk
                    // carries a cursor.
                    not (PathLimits.spliceWithinLimit limits linkTarget rest)
                then
                    Error UnixError.ENAMETOOLONG
                else

                let linkPath = SymlinkTarget.toUnixPath linkTarget

                let next = if UnixPath.isRooted linkPath then vfs.Root else directory

                // The link's own trailing separator only takes effect when
                // nothing follows it: when the walk has more to resolve, the
                // separator joining the target to the remainder absorbs it.
                //
                // It *adds to* the outer demand rather than replacing it. The
                // separator in "ld/" applies to whatever ld expands to, so a
                // link with target "d" still has to land on a directory; and a
                // link with target "d/" imposes the demand even when the
                // guest's own path carried none.
                //
                // This demand is threaded rather than read back off the spliced
                // buffer, and must be: resolving "ld/" consumes the trailing
                // separator into the cursor (that is what the kernel's own
                // collapse does), so the spliced buffer is just the target and
                // has forgotten it. A kernel remembers the same fact the same
                // way — XNU latches `TRAILINGSLASH` on the component rather than
                // re-reading the buffer.
                let trailing =
                    if isFinal then
                        trailing || UnixPath.hasTrailingSeparator linkPath
                    else
                        trailing

                // Exactly what a kernel does to its pathname buffer: the target,
                // then whatever was left to resolve. Note this consumes `rest`,
                // whose cursor already sits past the separator run the kernel
                // collapsed — so the spliced buffer holds the same bytes the
                // kernel's would.
                let spliced = PathCursor.splice linkPath rest

                // An empty splice can only mean the target was "/", that being
                // the one path with no components; the effective path is then
                // the root itself rather than whatever navigation preceded the
                // link.
                let lastNavigation =
                    if PathCursor.isExhausted spliced then
                        FinalNavigation.Root
                    else
                        lastNavigation

                walk next spliced trailing (finalSymlinkFollowed || isFinal) lastNavigation (symlinks + 1)
            | InodeContent.Symlink _ ->
                // Final position under NoFollowFinal with no trailing
                // separator: the link itself is the answer, which is what
                // `lstat` and `readlink` need.
                finish (ResolvedTarget.Entry (directory, name, Some target))
            | InodeContent.Directory _ ->
                if isFinal then
                    finish (ResolvedTarget.Entry (directory, name, Some target))
                else
                    walk target rest trailing finalSymlinkFollowed lastNavigation symlinks
            | InodeContent.RegularFile _ ->
                if isFinal then
                    // The one part of the trailing-separator rule every platform
                    // agrees on: "p/" where p exists and is not a directory is
                    // ENOTDIR.
                    if trailing then
                        Error UnixError.ENOTDIR
                    else
                        finish (ResolvedTarget.Entry (directory, name, Some target))
                else
                    // A path cannot continue through a regular file.
                    Error UnixError.ENOTDIR

        walk start (PathCursor.ofPath path) (UnixPath.hasTrailingSeparator path) false FinalNavigation.Root 0

    /// `resolveFull`, discarding the how-it-finished facts. For the lookup
    /// operations, which are unanimous across platforms and so need none of
    /// them.
    let resolve
        (limits : PathLimits)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<ResolvedTarget, UnixError>
        =
        resolveFull limits startDirectory policy path vfs
        |> Result.map (fun resolution -> resolution.Target)

    /// The inode a path names, which is what `stat` and `open` want. Turns a
    /// free final name into ENOENT, which is the one thing `resolve`
    /// deliberately does not do.
    let resolveExisting
        (limits : PathLimits)
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber, UnixError>
        =
        match resolve limits startDirectory policy path vfs with
        | Error error -> Error error
        | Ok (ResolvedTarget.Directory (inode, _)) -> Ok inode
        | Ok (ResolvedTarget.Entry (_, _, Some inode)) -> Ok inode
        | Ok (ResolvedTarget.Entry (_, _, None)) -> Error UnixError.ENOENT

    // ------------------------------------------------------------ inspection

    /// Every (directory, name, target) binding in the graph, including those in
    /// directories nothing can reach.
    let private allBindings (vfs : VirtualFileSystem) : (InodeNumber * FileName * InodeNumber) list =
        vfs.Inodes
        |> Map.toList
        |> List.collect (fun (inode, entry) ->
            match entry.Content with
            | InodeContent.Directory directory ->
                directory.Entries
                |> Map.toList
                |> List.map (fun (name, target) -> inode, name, target)
            | InodeContent.RegularFile _
            | InodeContent.Symlink _ -> []
        )

    /// The absolute path of a directory, by walking `Parent` links to the root.
    ///
    /// Directories only: a regular file may be hard-linked under several names,
    /// so it has no single path, and answering with one of them would be a
    /// guess dressed up as a fact. `None` if `inode` is absent, is not a
    /// directory, or sits in a graph whose parent links do not reach the root —
    /// including one whose parent links cycle, which the visited set bounds so
    /// that this stays total on a defective graph (it is used as a test oracle,
    /// and defective graphs are exactly what those tests construct).
    let pathOfDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : AbsoluteUnixPath option =
        let rec climb (current : InodeNumber) (acc : FileName list) (visited : Set<InodeNumber>) =
            if current = vfs.Root then
                Some acc
            elif Set.contains current visited then
                None
            else

            match tryGetDirectory current vfs with
            | None -> None
            | Some content ->

            // The name is not stored on the inode, so recover it from the
            // parent's own entries. A well-formed graph has exactly one.
            match
                tryGetDirectory content.Parent vfs
                |> Option.bind (fun parent ->
                    parent.Entries
                    |> Map.toList
                    |> List.tryPick (fun (name, target) -> if target = current then Some name else None)
                )
            with
            | None -> None
            | Some name -> climb content.Parent (name :: acc) (Set.add current visited)

        match tryGetDirectory inode vfs with
        | None -> None
        | Some _ ->

        climb inode [] Set.empty
        |> Option.map (fun names ->
            let rendered =
                names
                |> List.map FileName.toString
                |> List.fold (fun acc name -> acc + string UnixPathText.separator + name) ""

            if rendered = "" then
                AbsoluteUnixPath.root
            else
                AbsoluteUnixPath.parseOrFail "VirtualFileSystem.pathOfDirectory" rendered
        )

    /// Every way in which `vfs` fails to describe a filesystem a kernel could
    /// produce, or the empty list if it is sound. Deterministic in order, so a
    /// failing test reports the same thing every run.
    ///
    /// Together, the link-count and reachability rules make tree-ness a
    /// theorem rather than a further check: the root has no incoming entry
    /// link and every other directory has exactly one, so any cycle among
    /// reachable directories would force some directory to have two, and any
    /// cycle that avoids that is unreachable from the root and flagged as such.
    let checkInvariants (vfs : VirtualFileSystem) : VirtualFileSystemDefect list =
        let bindings = allBindings vfs

        let rootDefects =
            match tryGetContent vfs.Root vfs with
            | None -> [ VirtualFileSystemDefect.RootMissing vfs.Root ]
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> [ VirtualFileSystemDefect.RootIsNotDirectory vfs.Root ]
            | Some (InodeContent.Directory content) ->
                if content.Parent = vfs.Root then
                    []
                else
                    [ VirtualFileSystemDefect.RootParentIsNotSelf (vfs.Root, content.Parent) ]

        let danglingEntries =
            bindings
            |> List.filter (fun (_, _, target) -> not (Map.containsKey target vfs.Inodes))
            |> List.map VirtualFileSystemDefect.DanglingEntry

        /// Which directories hold each inode, so that the link-count rules and
        /// "the recorded parent is the real one" can all be decided.
        let holders : Map<InodeNumber, (InodeNumber * FileName) list> =
            bindings
            |> List.fold
                (fun acc (directory, name, target) ->
                    let existing = Map.tryFind target acc |> Option.defaultValue []
                    Map.add target ((directory, name) :: existing) acc
                )
                Map.empty
            |> Map.map (fun _ holders -> List.rev holders)

        let rootLinks =
            match Map.tryFind vfs.Root holders with
            | None -> []
            | Some parents -> [ VirtualFileSystemDefect.RootHasIncomingLink parents ]

        let parentDefects =
            vfs.Inodes
            |> Map.toList
            |> List.collect (fun (inode, entry) ->
                match entry.Content with
                | InodeContent.RegularFile _
                | InodeContent.Symlink _ -> []
                | InodeContent.Directory directory ->

                // The root's parent is checked above, where "is itself" is the
                // rule rather than "is whoever holds it".
                if inode = vfs.Root then
                    []
                else

                let recorded = directory.Parent

                let structural =
                    match tryGetContent recorded vfs with
                    | None -> [ VirtualFileSystemDefect.DanglingParent (inode, recorded) ]
                    | Some (InodeContent.RegularFile _)
                    | Some (InodeContent.Symlink _) ->
                        [ VirtualFileSystemDefect.ParentIsNotDirectory (inode, recorded) ]
                    | Some (InodeContent.Directory _) -> []

                match Map.tryFind inode holders with
                | None ->
                    // Held by nothing: reported as unreachable below, and there
                    // is no actual parent to disagree with.
                    structural
                | Some [ (actual, _) ] ->
                    if actual = recorded then
                        structural
                    else
                        structural
                        @ [ VirtualFileSystemDefect.ParentMismatch (inode, recorded, actual) ]
                | Some holders ->
                    structural
                    @ [ VirtualFileSystemDefect.DirectoryMultiplyLinked (inode, holders) ]
            )

        let reachable =
            // Breadth-first from the root through directory entries only.
            // Parent links deliberately do not count: a directory reachable
            // only by climbing out of an orphaned subtree is still orphaned.
            let rec explore (frontier : InodeNumber list) (seen : Set<InodeNumber>) : Set<InodeNumber> =
                match frontier with
                | [] -> seen
                | inode :: rest ->
                    if Set.contains inode seen then
                        explore rest seen
                    else

                    let children =
                        match tryGetContent inode vfs with
                        | Some (InodeContent.Directory directory) -> directory.Entries |> Map.toList |> List.map snd
                        | Some _
                        | None -> []

                    explore (children @ rest) (Set.add inode seen)

            if Map.containsKey vfs.Root vfs.Inodes then
                explore [ vfs.Root ] Set.empty
            else
                Set.empty

        let unreachable =
            vfs.Inodes
            |> Map.toList
            |> List.map fst
            |> List.filter (fun inode -> not (Set.contains inode reachable))
            |> List.map VirtualFileSystemDefect.UnreachableFromRoot

        let freshness =
            vfs.Inodes
            |> Map.toList
            |> List.map fst
            |> List.filter (fun inode -> inode >= vfs.NextInode)
            |> List.map (fun inode -> VirtualFileSystemDefect.NextInodeNotFresh (vfs.NextInode, inode))

        rootDefects
        @ rootLinks
        @ danglingEntries
        @ parentDefects
        @ unreachable
        @ freshness

    /// Fail loudly if `vfs` is not sound, naming `context`. For the operations
    /// that build a filesystem from host configuration, where a defect is a
    /// host bug rather than anything a guest could have caused.
    let assertInvariants (context : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        match checkInvariants vfs with
        | [] -> vfs
        | defects ->
            let rendered = defects |> List.map (sprintf "%A") |> String.concat "; "
            failwith $"%s{context}: the inode graph is not a filesystem any kernel could produce: %s{rendered}"

    /// Construction that bypasses every invariant this module maintains.
    ///
    /// Exists so that `checkInvariants` can be tested: a defect no test can
    /// construct is documentation rather than a check. Deliberately one
    /// greppable token, so that any interpreter code reaching for it is visible
    /// in review — nothing outside tests should.
    [<RequireQualifiedAccess>]
    module Unchecked =
        let ofParts
            (inodes : Map<InodeNumber, Inode>)
            (root : InodeNumber)
            (nextInode : InodeNumber)
            : VirtualFileSystem
            =
            {
                Inodes = inodes
                Root = root
                NextInode = nextInode
            }
