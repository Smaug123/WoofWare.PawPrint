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
/// bytes unchanged, and `lstat` reports their length as the link's `st_size`.
/// `UnixPath.parse` deliberately collapses repeated separators, so a link
/// created with target "a//b/" would read back as "a/b" — a difference a guest
/// really can see, through `FileInfo.LinkTarget` and `ResolveLinkTarget`.
///
/// The path structure is recovered by parsing at traversal time, which is
/// cheap, total, and keeps the stored form authoritative.
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
    }

/// What lives at an inode. The `S_IFMT` file-type bits a guest reads from
/// `stat` are *derived* from which case this is, never stored, so the two can
/// never disagree.
///
/// Deliberately carries no metadata — no mode, owner, or timestamps. Those
/// arrive with the `stat` family, which is also when the emulated kernel's
/// clock becomes available to give timestamps a value that is not invented.
/// Until then there is no field to read, so a caller that needs one fails to
/// compile rather than silently observing a plausible-looking default that no
/// real filesystem would have produced.
///
/// Names are compared with F#'s ordinal string comparison, so the emulated
/// filesystem is case-sensitive and normalisation-preserving. That is not a
/// platform divergence to crash on: case-sensitivity is a property of a
/// *filesystem* rather than of an OS (Linux mounts case-insensitive
/// directories; macOS runs case-sensitive APFS). It does mean the model
/// resembles a Linux default rather than a macOS one.
[<RequireQualifiedAccess>]
type InodeContent =
    | RegularFile of contents : ImmutableArray<byte>
    | Directory of directory : DirectoryContent
    /// The link's target, unresolved: a symlink's target is a *string* to the
    /// kernel, re-resolved on every traversal, not a reference to whatever it
    /// pointed at when it was made.
    | Symlink of target : SymlinkTarget

/// A whole emulated filesystem: an inode graph rooted at a single directory.
type VirtualFileSystem =
    private
        {
            Inodes : Map<InodeNumber, InodeContent>
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

[<RequireQualifiedAccess>]
module VirtualFileSystem =
    /// The number of symlink traversals below which every Unix PawPrint models
    /// agrees the resolution should proceed. macOS's `MAXSYMLINKS` is 32
    /// (`sys/param.h`, and probed: a chain of 32 resolves, 33 gives ELOOP);
    /// Linux's is 40 (`MAXSYMLINKS` in `include/linux/namei.h`, a
    /// kernel-internal header rather than a UAPI one).
    [<Literal>]
    let symlinksEveryPlatformAllows : int = 32

    /// The number of symlink traversals at which every Unix PawPrint models
    /// agrees the resolution fails with ELOOP. Linux permits 40 and fails the
    /// 41st attempt.
    ///
    /// Also the bound that makes the walk terminate, which is why there is no
    /// cycle detection here: a seen-state set is *not* sufficient, because a
    /// link whose target names itself with a suffix ("l" with target "l/x")
    /// grows the remaining component list forever without ever repeating a
    /// state. Only a count stops that — and both real kernels use only a
    /// counter too, so this is not an approximation of their behaviour but a
    /// transcription of it.
    [<Literal>]
    let symlinksNoPlatformAllows : int = 41

    /// Inode 1, matching the convention that no real filesystem hands out inode
    /// 0. A zero default would otherwise silently alias whichever inode was
    /// allocated first.
    let private firstInode : InodeNumber = InodeNumber 1L

    /// A filesystem containing nothing but an empty root directory.
    let empty : VirtualFileSystem =
        {
            Inodes =
                Map.ofList
                    [
                        firstInode,
                        InodeContent.Directory
                            {
                                Entries = Map.empty
                                Parent = firstInode
                            }
                    ]
            Root = firstInode
            NextInode = InodeNumber 2L
        }

    let root (vfs : VirtualFileSystem) : InodeNumber = vfs.Root

    let nextInode (vfs : VirtualFileSystem) : InodeNumber = vfs.NextInode

    let inodes (vfs : VirtualFileSystem) : Map<InodeNumber, InodeContent> = vfs.Inodes

    let tryGet (inode : InodeNumber) (vfs : VirtualFileSystem) : InodeContent option = Map.tryFind inode vfs.Inodes

    /// The directory at `inode`, or `None` if it is absent or is not a
    /// directory. Honest about which: callers that must distinguish ENOENT from
    /// ENOTDIR use `tryGet` and match.
    let private tryGetDirectory (inode : InodeNumber) (vfs : VirtualFileSystem) : DirectoryContent option =
        match Map.tryFind inode vfs.Inodes with
        | Some (InodeContent.Directory directory) -> Some directory
        | Some _
        | None -> None

    let private allocate (content : InodeContent) (vfs : VirtualFileSystem) : InodeNumber * VirtualFileSystem =
        let inode = vfs.NextInode
        let (InodeNumber raw) = inode

        let vfs =
            { vfs with
                Inodes = Map.add inode content vfs.Inodes
                NextInode = InodeNumber (raw + 1L)
            }

        inode, vfs

    /// Bind `name` to `inode` in `directory`, which must exist, be a directory,
    /// and not already hold `name`.
    let private bind
        (directory : InodeNumber)
        (name : FileName)
        (inode : InodeNumber)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        // Every builder binds through here, so this is the one place a name
        // enters the graph — and the one place a forged `default(FileName)` can
        // be stopped before it becomes an entry no path could ever name.
        let name = FileName.assertValid "VirtualFileSystem: directory entry name" name

        match Map.tryFind directory vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR
        | Some (InodeContent.Directory content) ->
            if Map.containsKey name content.Entries then
                Error UnixError.EEXIST
            else

            let updated =
                InodeContent.Directory
                    { content with
                        Entries = Map.add name inode content.Entries
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
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        // Allocate first so the new directory can record its parent, but bind
        // second so a rejected bind leaves nothing but a burnt inode number —
        // which is unobservable, since numbers are never reused anyway.
        let inode, allocated =
            allocate
                (InodeContent.Directory
                    {
                        Entries = Map.empty
                        Parent = directory
                    })
                vfs

        bind directory name inode allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a regular file with the given contents. Mirrors `open(2)` with
    /// `O_CREAT | O_EXCL`.
    let createFile
        (directory : InodeNumber)
        (name : FileName)
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

        let inode, allocated = allocate (InodeContent.RegularFile contents) vfs
        bind directory name inode allocated |> Result.map (fun vfs -> inode, vfs)

    /// Create a symbolic link holding `target` verbatim. Mirrors `symlink(2)`,
    /// including that the target is not resolved, need not exist, and may be
    /// relative. An empty target is unrepresentable by construction; see
    /// `SymlinkTargetError.Empty` for why that is a refusal rather than an
    /// omission.
    let createSymlink
        (directory : InodeNumber)
        (name : FileName)
        (target : SymlinkTarget)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber * VirtualFileSystem, UnixError>
        =
        let target = SymlinkTarget.assertValid "VirtualFileSystem.createSymlink" target
        let inode, allocated = allocate (InodeContent.Symlink target) vfs
        bind directory name inode allocated |> Result.map (fun vfs -> inode, vfs)

    /// Bind an existing inode under a second name. Mirrors `link(2)`, including
    /// its refusal to hard-link a directory (EPERM): that would make the graph
    /// a non-tree, and a directory's `Parent` could then name only one of its
    /// containers.
    let hardLink
        (directory : InodeNumber)
        (name : FileName)
        (target : InodeNumber)
        (vfs : VirtualFileSystem)
        : Result<VirtualFileSystem, UnixError>
        =
        match Map.tryFind target vfs.Inodes with
        | None -> Error UnixError.ENOENT
        | Some (InodeContent.Directory _) -> Error UnixError.EPERM
        | Some (InodeContent.RegularFile _)
        | Some (InodeContent.Symlink _) -> bind directory name target vfs

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
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<Resolution, UnixError>
        =
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

            match Map.tryFind startDirectory vfs.Inodes with
            | None -> Error UnixError.ENOENT
            | Some (InodeContent.Directory _) -> Ok startDirectory
            | Some (InodeContent.RegularFile _)
            | Some (InodeContent.Symlink _) -> Error UnixError.ENOTDIR

        match start with
        | Error error -> Error error
        | Ok start ->

        /// The walk returns the trailing-separator demand and whether it
        /// followed a final symlink alongside its outcome, plus the running
        /// symlink count that decides whether the answer is reportable at all.
        let rec walk
            (directory : InodeNumber)
            (remaining : PathComponent list)
            (trailing : bool)
            (finalSymlinkFollowed : bool)
            (lastNavigation : FinalNavigation)
            (symlinks : int)
            : Result<Resolution, UnixError> * int
            =
            match remaining with
            // Reached when the path has no name left to look up: after a "." or
            // "..", or immediately for a path that named no component at all.
            | [] ->
                Ok
                    {
                        Target = ResolvedTarget.Directory (directory, lastNavigation)
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    },
                symlinks
            | PathComponent.Current :: rest ->
                walk directory rest trailing finalSymlinkFollowed FinalNavigation.Current symlinks
            | PathComponent.Parent :: rest ->
                walk (parentOf directory vfs) rest trailing finalSymlinkFollowed FinalNavigation.Parent symlinks
            | PathComponent.Name name :: rest ->

            let entries =
                match tryGetDirectory directory vfs with
                | Some content -> content.Entries
                | None ->
                    failwith
                        $"VirtualFileSystem: looking up \"%s{FileName.toString name}\" in inode %O{directory}, which the walk had already established was a directory, but it is now absent or not a directory. The inode graph is inconsistent; run VirtualFileSystem.checkInvariants."

            let isFinal = List.isEmpty rest

            let finish (target : ResolvedTarget) =
                Ok
                    {
                        Target = target
                        TrailingSeparatorDemanded = trailing
                        FinalSymlinkFollowed = finalSymlinkFollowed
                    },
                symlinks

            match Map.tryFind name entries with
            | None ->
                if isFinal then
                    // Not an error: the caller decides whether a free name is
                    // ENOENT (`stat`) or the point of the call (`mkdir`). A
                    // trailing separator does not change that — `mkdir("nx/")`
                    // creates on both platforms.
                    finish (ResolvedTarget.Entry (directory, name, None))
                else
                    Error UnixError.ENOENT, symlinks
            | Some target ->

            let content =
                match Map.tryFind target vfs.Inodes with
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
                if symlinks + 1 >= symlinksNoPlatformAllows then
                    // Every platform has given up by here, so this is an answer
                    // rather than a divergence.
                    Error UnixError.ELOOP, symlinks + 1
                else

                let linkPath = SymlinkTarget.toUnixPath linkTarget

                let next = if UnixPath.isRooted linkPath then vfs.Root else directory

                // The link's own trailing separator only takes effect when
                // nothing follows it: a separator between the target and the
                // remainder absorbs it, exactly as `UnixPath.concat` describes.
                //
                // It *adds to* the outer demand rather than replacing it. The
                // separator in "ld/" applies to whatever ld expands to, so a
                // link with target "d" still has to land on a directory; and a
                // link with target "d/" imposes the demand even when the
                // guest's own path carried none.
                let trailing =
                    if isFinal then
                        trailing || UnixPath.hasTrailingSeparator linkPath
                    else
                        trailing

                let spliced = UnixPath.components linkPath @ rest

                // An empty splice can only mean the target was "/", that being
                // the one path with no components; the effective path is then
                // the root itself rather than whatever navigation preceded the
                // link.
                let lastNavigation =
                    if List.isEmpty spliced then
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
                        Error UnixError.ENOTDIR, symlinks
                    else
                        finish (ResolvedTarget.Entry (directory, name, Some target))
                else
                    // A path cannot continue through a regular file.
                    Error UnixError.ENOTDIR, symlinks

        let outcome, symlinks =
            walk start (UnixPath.components path) (UnixPath.hasTrailingSeparator path) false FinalNavigation.Root 0

        if symlinks <= symlinksEveryPlatformAllows then
            outcome
        elif symlinks >= symlinksNoPlatformAllows then
            // The walk stopped at the bound, so ELOOP is what every platform
            // would have said, and `walk` has already returned it.
            outcome
        else
            // Note this fires for a *failed* walk too, not only a successful
            // one. A 35-link chain ending at a missing name gives ENOENT on
            // Linux and ELOOP on macOS: once the 33rd traversal has happened,
            // ELOOP is the only outcome any platform could still be made to
            // agree on, so every other answer is a divergence.
            failwith
                $"VirtualFileSystem.resolveFull: resolving %s{UnixPath.toString path} traversed %d{symlinks} symlinks, which Linux permits (its MAXSYMLINKS is 40) and macOS does not (its MAXSYMLINKS is 32), so the two disagree about whether this call succeeds or fails with ELOOP. PawPrint refuses to pick one. Give the resolution limit a value derived from the emulated kernel's SimulatedUnixPlatform when a guest genuinely needs a symlink chain this deep."

    /// `resolveFull`, discarding the how-it-finished facts. For the lookup
    /// operations, which are unanimous across platforms and so need none of
    /// them.
    let resolve
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<ResolvedTarget, UnixError>
        =
        resolveFull startDirectory policy path vfs
        |> Result.map (fun resolution -> resolution.Target)

    /// The inode a path names, which is what `stat` and `open` want. Turns a
    /// free final name into ENOENT, which is the one thing `resolve`
    /// deliberately does not do.
    let resolveExisting
        (startDirectory : InodeNumber)
        (policy : SymlinkPolicy)
        (path : UnixPath)
        (vfs : VirtualFileSystem)
        : Result<InodeNumber, UnixError>
        =
        match resolve startDirectory policy path vfs with
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
        |> List.collect (fun (inode, content) ->
            match content with
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
            match Map.tryFind vfs.Root vfs.Inodes with
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
            |> List.collect (fun (inode, content) ->
                match content with
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
                    match Map.tryFind recorded vfs.Inodes with
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
                        match Map.tryFind inode vfs.Inodes with
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
            (inodes : Map<InodeNumber, InodeContent>)
            (root : InodeNumber)
            (nextInode : InodeNumber)
            : VirtualFileSystem
            =
            {
                Inodes = inodes
                Root = root
                NextInode = nextInode
            }
