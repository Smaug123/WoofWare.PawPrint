namespace WoofWare.PosixKernel

/// What `access(2)` asks of the inode a path names: the `R_OK`, `W_OK` and
/// `X_OK` bits of its mode word, which are 4, 2 and 1 on every modelled Unix.
///
/// All three false is `F_OK`, which asks only that the path resolve.
type AccessQuestion =
    {
        /// `R_OK`: may the caller read it?
        Read : bool
        /// `W_OK`: may the caller write it?
        Write : bool
        /// `X_OK`: may the caller search it, if it is a directory, or execute
        /// it, if it is anything else?
        Execute : bool
    }

[<RequireQualifiedAccess>]
module AccessQuestion =
    /// `F_OK`: asks nothing of the inode, only that the path resolve.
    let exists : AccessQuestion =
        {
            Read = false
            Write = false
            Execute = false
        }

    /// The question the low three bits of a mode word ask: `R_OK` is 4,
    /// `W_OK` 2 and `X_OK` 1. Every other bit of `bits` is ignored.
    let ofLowBits (bits : int) : AccessQuestion =
        {
            Read = bits &&& 4 <> 0
            Write = bits &&& 2 <> 0
            Execute = bits &&& 1 <> 0
        }

/// Which of a process's IDs `faccessat(2)` checks a path with.
[<RequireQualifiedAccess>]
type AccessIds =
    /// The real user and group: `access(2)`, and `faccessat` without
    /// `AT_EACCESS`. See `Credentials.realIdsAsEffective`.
    | Real
    /// The effective user and group, as every other syscall uses: `faccessat`
    /// with `AT_EACCESS`.
    | Effective

/// The directory a relative path given to a `*at` syscall starts from: its
/// `dirfd` argument, decoded.
[<RequireQualifiedAccess>]
type AtDirectory =
    /// `AT_FDCWD`: the process's current directory.
    | CurrentDirectory
    /// Any other value, which the call looks up in the descriptor table if it
    /// needs a starting directory at all.
    | Descriptor of fd : int

/// What `faccessat(2)` does with an empty path.
[<RequireQualifiedAccess>]
type AccessEmptyPath =
    /// The call asks about the object `dirfd` names itself, which can be a
    /// regular file as well as a directory. Linux's `AT_EMPTY_PATH`.
    | NamesStartingPoint
    /// ENOENT, before `dirfd` is looked at, so a `dirfd` that names nothing
    /// does not matter. Linux without `AT_EMPTY_PATH`.
    | NoSuchEntryBeforeDescriptor
    /// ENOENT, but only once `dirfd` has been found to name a directory: a
    /// `dirfd` naming nothing is EBADF, and one naming a regular file is
    /// ENOTDIR. Darwin.
    | NoSuchEntryAfterDescriptor

/// The arguments of `faccessat(2)` other than its path and `dirfd`, once this
/// kernel has screened its mode word and flag word.
type AccessArguments =
    {
        /// What the call asks of the inode.
        Question : AccessQuestion
        /// Bits of the mode word asking for rights this library does not model,
        /// which the call is refused for once its path has resolved. Only
        /// Darwin has any: its `_READ_OK` to `_CHOWN_OK`, bits 9 to 21.
        ExtendedRights : int
        /// Which IDs the path is walked and the inode judged with.
        Ids : AccessIds
        /// Whether a symbolic link in the final position is followed:
        /// `AT_SYMLINK_NOFOLLOW` says not.
        FinalSymlink : SymlinkPolicy
        /// What the call makes of an empty path.
        EmptyPath : AccessEmptyPath
    }

/// Why this kernel will not answer an `access(2)` or `faccessat(2)`.
[<RequireQualifiedAccess>]
type AccessRefusal =
    /// The flag word carries flags this kernel accepts but whose meaning this
    /// library does not model: Darwin's `AT_SYMLINK_NOFOLLOW_ANY` (0x800),
    /// `AT_RESOLVE_BENEATH` (0x2000) and `AT_UNIQUE` (0x8000). `flags` is the
    /// whole word.
    | UnmodelledFlags of flags : int
    /// The mode word asks for Darwin's extended rights (`rights` is those bits
    /// of it) of the inode at `inode`, which this library does not model.
    | ExtendedRights of inode : InodeNumber * rights : int
    /// Whether the caller may execute the inode at `inode` has not been
    /// measured for this caller.
    | UnmeasuredExecution of inode : InodeNumber * refusal : ExecutionRefusal
    /// The `dirfd` names something other than a directory or a regular file,
    /// and the call would start from it.
    | UnmodelledDescriptor of fd : int
    /// This kernel will not resolve the path.
    | Path of PathRefusal

[<RequireQualifiedAccess>]
module AccessRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which path.
    let describe (refusal : AccessRefusal) : string =
        match refusal with
        | AccessRefusal.UnmodelledFlags flags ->
            $"the flag word 0x%x{flags} carries a flag Darwin accepts and this library does not model: AT_SYMLINK_NOFOLLOW_ANY (0x800), AT_RESOLVE_BENEATH (0x2000) or AT_UNIQUE (0x8000). Each changes how the path is walked; model the walk before answering."
        | AccessRefusal.ExtendedRights (inode, rights) ->
            $"the mode word asks for Darwin's extended rights 0x%x{rights} of inode %O{inode} (_READ_OK to _CHOWN_OK, bits 9 to 21), which this library does not model. Measured as the owner, some are refused on a mode-0 file and _CHOWN_OK even on a 0777 one, so they are questions of their own rather than the permission bits restated."
        | AccessRefusal.UnmeasuredExecution (inode, refusal) ->
            $"executing inode %O{inode}: %s{ExecutionRefusal.describe refusal}"
        | AccessRefusal.UnmodelledDescriptor fd ->
            $"fd %d{fd} names neither a directory nor a regular file, and the call would start from it. What a kernel answers for a pipe (the standard streams among them), a socket or an event queue there has not been measured."
        | AccessRefusal.Path refusal -> PathRefusal.describe refusal

/// What screening `faccessat(2)`'s mode word and flag word came to.
[<RequireQualifiedAccess>]
type AccessScreen =
    /// Both words are ones this kernel accepts, and this is what they say.
    | Screened of AccessArguments
    /// The call fails with this errno before its path is copied in.
    | Failed of error : UnixError
    /// This library will not answer the call at all.
    | Refused of refusal : AccessRefusal

[<RequireQualifiedAccess>]
module AccessRules =

    // `<fcntl.h>`'s numbering, measured by `access-rules.c` on Linux 6.18.5
    // and Darwin 27.0.
    let private linuxAtFdCwd : int = -100
    let private linuxAtSymlinkNoFollow : int = 0x100
    let private linuxAtEAccess : int = 0x200
    let private linuxAtEmptyPath : int = 0x1000
    let private darwinAtFdCwd : int = -2
    let private darwinAtEAccess : int = 0x10
    let private darwinAtSymlinkNoFollow : int = 0x20
    // AT_SYMLINK_NOFOLLOW_ANY, AT_RESOLVE_BENEATH and AT_UNIQUE.
    let private darwinUnmodelledFlags : int = 0x800 ||| 0x2000 ||| 0x8000
    // `_ACCESS_EXTENDED_MASK`: `_READ_OK` (1 << 9) to `_CHOWN_OK` (1 << 21).
    let private darwinExtendedRights : int = 0x3FFE00

    /// What a `*at` syscall's raw `dirfd` names under `flavour`: its own
    /// `AT_FDCWD` (-100 on Linux, -2 on Darwin), or a descriptor. Each
    /// flavour's `AT_FDCWD` is merely a descriptor number nothing holds under
    /// the other.
    let atDirectory (flavour : SimulatedUnixFlavour) (dirfd : int) : AtDirectory =
        let atFdCwd =
            match flavour with
            | SimulatedUnixFlavour.Linux -> linuxAtFdCwd
            | SimulatedUnixFlavour.Darwin -> darwinAtFdCwd

        if dirfd = atFdCwd then
            AtDirectory.CurrentDirectory
        else
            AtDirectory.Descriptor dirfd

    /// Screen `faccessat(2)`'s raw mode word and flag word as `flavour` does,
    /// before the path is copied in.
    ///
    /// Linux answers EINVAL for any mode bit beyond the low three, and then for
    /// any flag beyond `AT_SYMLINK_NOFOLLOW` (0x100), `AT_EACCESS` (0x200) and
    /// `AT_EMPTY_PATH` (0x1000). Darwin answers EINVAL for any flag beyond
    /// `AT_EACCESS` (0x10) and `AT_SYMLINK_NOFOLLOW` (0x20) and the three it
    /// accepts but this library refuses (see `AccessRefusal.UnmodelledFlags`);
    /// it rejects no mode word, ignores bits 3 to 8 and 22 to 31, and reads
    /// bits 9 to 21 as its extended rights.
    let screen (flavour : SimulatedUnixFlavour) (mode : int) (flags : int) : AccessScreen =
        // Measured by `access-rules.c`: every single bit of each word, and
        // the order of a bad mode, bad flags, a bad dirfd and the path's
        // copy-in against one another. Linux: a bad mode is EINVAL ahead of
        // bad flags, and both ahead of an unreadable or over-long path and a
        // bad dirfd. Darwin: bad flags are EINVAL ahead of the same; bits 3
        // to 8 and 22 to 31 changed no answer over every permission word of a
        // file and a directory the caller owned.
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            if mode &&& ~~~0o7 <> 0 then
                AccessScreen.Failed UnixError.EINVAL
            elif
                flags &&& ~~~(linuxAtSymlinkNoFollow ||| linuxAtEAccess ||| linuxAtEmptyPath)
                <> 0
            then
                AccessScreen.Failed UnixError.EINVAL
            else

            AccessScreen.Screened
                {
                    Question = AccessQuestion.ofLowBits mode
                    ExtendedRights = 0
                    Ids =
                        if flags &&& linuxAtEAccess <> 0 then
                            AccessIds.Effective
                        else
                            AccessIds.Real
                    FinalSymlink =
                        if flags &&& linuxAtSymlinkNoFollow <> 0 then
                            SymlinkPolicy.NoFollowFinal
                        else
                            SymlinkPolicy.Follow
                    EmptyPath =
                        if flags &&& linuxAtEmptyPath <> 0 then
                            AccessEmptyPath.NamesStartingPoint
                        else
                            AccessEmptyPath.NoSuchEntryBeforeDescriptor
                }
        | SimulatedUnixFlavour.Darwin ->
            // Refused whatever else the word carries: which of EINVAL and these
            // flags' own answers wins when both are present is unmeasured.
            if flags &&& darwinUnmodelledFlags <> 0 then
                AccessScreen.Refused (AccessRefusal.UnmodelledFlags flags)
            elif flags &&& ~~~(darwinAtEAccess ||| darwinAtSymlinkNoFollow) <> 0 then
                AccessScreen.Failed UnixError.EINVAL
            else

            AccessScreen.Screened
                {
                    Question = AccessQuestion.ofLowBits mode
                    ExtendedRights = mode &&& darwinExtendedRights
                    Ids =
                        if flags &&& darwinAtEAccess <> 0 then
                            AccessIds.Effective
                        else
                            AccessIds.Real
                    FinalSymlink =
                        if flags &&& darwinAtSymlinkNoFollow <> 0 then
                            SymlinkPolicy.NoFollowFinal
                        else
                            SymlinkPolicy.Follow
                    EmptyPath = AccessEmptyPath.NoSuchEntryAfterDescriptor
                }

    /// <summary>
    /// Whether a caller standing as <c>standing</c> towards an inode whose content is
    /// <c>content</c> and whose permission bits are <c>bits</c> is refused what
    /// <c>question</c> asks, which <c>access(2)</c> answers EACCES.
    /// </summary>
    /// <remarks>
    /// Each bit asked is judged alone, and the call is refused if any one of them is.
    /// Read and write are <c>PermissionBits.deniedTo</c>'s. Execute is search on a
    /// directory, which <c>deniedTo</c> answers too, and on anything else is
    /// <c>PermissionBits.executionDenied</c>'s under <c>rule</c>, which is where a
    /// privileged caller can be refused, or refused an answer. <c>F_OK</c> is refused
    /// nothing.
    /// </remarks>
    let denied
        (rule : PrivilegedExecution)
        (standing : Standing)
        (content : InodeContent)
        (bits : PermissionBits)
        (question : AccessQuestion)
        : Result<bool, ExecutionRefusal>
        =
        // Measured by `access-rules.c` against exactly this rule, with no
        // mismatch: Linux 6.18.5 over all 4096 modes, every one of the eight
        // questions, on a file, a directory and a symbolic link asked about
        // itself, for the owner, a member of the group by its effective gid
        // and by a supplementary group, anyone else, and root; Darwin 27.0 at
        // uid 501 over the same for the owner, and its symbolic links over
        // every mode `lchmod` gives them.
        let execute : Result<bool, ExecutionRefusal> =
            if not question.Execute then
                Ok false
            else

            match content with
            | InodeContent.Directory _ -> Ok (PermissionBits.deniedTo standing AccessRequest.SearchDirectory bits)
            | InodeContent.RegularFile _
            | InodeContent.CharacterDevice _
            | InodeContent.Symlink _ -> PermissionBits.executionDenied rule standing bits

        match execute with
        | Error refusal -> Error refusal
        | Ok executeDenied ->

        Ok (
            executeDenied
            || question.Read && PermissionBits.deniedTo standing AccessRequest.Read bits
            || question.Write && PermissionBits.deniedTo standing AccessRequest.Write bits
        )
