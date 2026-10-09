namespace WoofWare.PosixKernel

/// The flag word of `fstatat(2)`, once this kernel has screened it.
type internal StatArguments =
    {
        /// Whether a symbolic link in the final position is followed:
        /// `AT_SYMLINK_NOFOLLOW` says not.
        FinalSymlink : SymlinkPolicy
        /// What the call makes of an empty path: Linux's `AT_EMPTY_PATH` asks
        /// about the object `dirfd` names, as `fstat(2)` would.
        EmptyPath : EmptyPathMeaning
    }

/// What screening `fstatat(2)`'s flag word came to.
[<RequireQualifiedAccess>]
type internal StatScreen =
    /// The word is one this kernel accepts, and this is what it says.
    | Screened of StatArguments
    /// The call fails with this errno before its path is copied in.
    | Failed of error : UnixError
    /// The word carries `flags`, which the flavour accepts and this library
    /// does not model.
    | Unmodelled of flags : int

[<RequireQualifiedAccess>]
module internal StatRules =

    // `<fcntl.h>`'s numbering, measured by `at-dirfd.c` (FLAGS) on Linux
    // 6.18.5 and Darwin 27.0.
    let private linuxAtSymlinkNoFollow : int = 0x100
    let private linuxAtEmptyPath : int = 0x1000
    // AT_NO_AUTOMOUNT, and AT_STATX_FORCE_SYNC and AT_STATX_DONT_SYNC.
    let private linuxUnmodelledFlags : int = 0x800 ||| 0x2000 ||| 0x4000
    let private darwinAtSymlinkNoFollow : int = 0x20
    // AT_REALDEV, AT_FDONLY, AT_SYMLINK_NOFOLLOW_ANY, AT_RESOLVE_BENEATH and
    // AT_UNIQUE.
    let private darwinUnmodelledFlags : int =
        0x200 ||| 0x400 ||| 0x800 ||| 0x2000 ||| 0x8000

    /// Screen `fstatat(2)`'s raw flag word as `flavour` does, before the path
    /// is copied in and before `dirfd` is looked at.
    ///
    /// Linux accepts `AT_SYMLINK_NOFOLLOW` (0x100) and `AT_EMPTY_PATH`
    /// (0x1000), and Darwin `AT_SYMLINK_NOFOLLOW` (0x20). Each answers EINVAL
    /// for any other flag, except those it accepts and this library does not
    /// model: Linux's `AT_NO_AUTOMOUNT` (0x800) and `AT_STATX_SYNC_TYPE` bits
    /// (0x2000, 0x4000), and Darwin's `AT_REALDEV` (0x200), `AT_FDONLY`
    /// (0x400), `AT_SYMLINK_NOFOLLOW_ANY` (0x800), `AT_RESOLVE_BENEATH`
    /// (0x2000) and `AT_UNIQUE` (0x8000). A word carrying any of those is
    /// `StatScreen.Unmodelled`, whatever else it carries.
    let screen (flavour : SimulatedUnixFlavour) (flags : int) : StatScreen =
        // Measured by `at-dirfd.c`: every single bit, with AT_FDCWD and a
        // regular file (FLAGS), and a rejected bit against an unreadable path,
        // a bad dirfd, an empty path and a missing name (FLAGORDER), which it
        // beats on both flavours. Which of EINVAL and an unmodelled flag's own
        // answer wins when a word carries both is unmeasured, so such a word
        // is refused.
        let accepted, unmodelled, noFollow, emptyPath =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                linuxAtSymlinkNoFollow ||| linuxAtEmptyPath,
                linuxUnmodelledFlags,
                linuxAtSymlinkNoFollow,
                linuxAtEmptyPath
            | SimulatedUnixFlavour.Darwin -> darwinAtSymlinkNoFollow, darwinUnmodelledFlags, darwinAtSymlinkNoFollow, 0

        if flags &&& unmodelled <> 0 then
            StatScreen.Unmodelled flags
        elif flags &&& ~~~accepted <> 0 then
            StatScreen.Failed UnixError.EINVAL
        else
            StatScreen.Screened
                {
                    FinalSymlink =
                        if flags &&& noFollow <> 0 then
                            SymlinkPolicy.NoFollowFinal
                        else
                            SymlinkPolicy.Follow
                    EmptyPath =
                        if flags &&& emptyPath <> 0 then
                            EmptyPathMeaning.NamesStartingPoint
                        else
                            EmptyPathMeaning.Walked
                }
