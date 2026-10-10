namespace WoofWare.PosixKernel

/// The flag word of `fchmodat(2)` or `fchownat(2)`, once this kernel has
/// screened it. The two calls accept the same flags on each flavour.
type internal AttributeChangeArguments =
    {
        /// Whether a symbolic link in the final position is followed:
        /// `AT_SYMLINK_NOFOLLOW` says not.
        FinalSymlink : SymlinkPolicy
        /// What the call makes of an empty path: Linux's `AT_EMPTY_PATH`
        /// changes the object `dirfd` names, as `fchmod(2)` or `fchown(2)`
        /// would.
        EmptyPath : EmptyPathMeaning
    }

/// What screening the flag word of `fchmodat(2)` or `fchownat(2)` came to.
[<RequireQualifiedAccess>]
type internal AttributeChangeScreen =
    /// The word is one this kernel accepts, and this is what it says.
    | Screened of AttributeChangeArguments
    /// The call fails with this errno before its path is copied in.
    | Failed of error : UnixError
    /// The word carries flags the flavour accepts and this library does not
    /// model: Darwin's `AT_SYMLINK_NOFOLLOW_ANY` (0x800), `AT_RESOLVE_BENEATH`
    /// (0x2000) and `AT_UNIQUE` (0x8000). `flags` is the whole word.
    | Unmodelled of flags : int

/// What a flavour's `fchmodat(2)` does when `AT_SYMLINK_NOFOLLOW` leaves it at
/// a symbolic link.
[<RequireQualifiedAccess>]
type SymlinkModeChange =
    /// EOPNOTSUPP, changing nothing, whoever asks: the link's owner, root, or
    /// a caller who could not have changed it anyway. Linux.
    | NotSupported
    /// The link's own mode changes, by the rule `chmod(2)` applies to a
    /// regular file (see `UnixPathResolution.chmod`), set-ID and sticky bits
    /// included, moving the link's `ctime`, even to the mode it already has,
    /// and nothing of its target's or its directory's. Darwin.
    | ChangesLink

[<RequireQualifiedAccess>]
module internal AttributeChangeRules =

    // `<fcntl.h>`'s numbering, measured by `at-dirfd.c` (CONST, FLAGS) on
    // Linux 6.18.5 and Darwin 27.0.
    let private linuxAtSymlinkNoFollow : int = 0x100
    let private linuxAtEmptyPath : int = 0x1000
    let private darwinAtSymlinkNoFollow : int = 0x20
    // AT_SYMLINK_NOFOLLOW_ANY, AT_RESOLVE_BENEATH and AT_UNIQUE.
    let private darwinUnmodelledFlags : int = 0x800 ||| 0x2000 ||| 0x8000

    /// Screen the raw flag word of `fchmodat(2)` or `fchownat(2)` as `flavour`
    /// does, before the path is copied in and before `dirfd` is looked at.
    ///
    /// Linux accepts `AT_SYMLINK_NOFOLLOW` (0x100) and `AT_EMPTY_PATH`
    /// (0x1000), and Darwin `AT_SYMLINK_NOFOLLOW` (0x20) and three flags this
    /// library does not model (see `AttributeChangeScreen.Unmodelled`). Each
    /// answers EINVAL for a word carrying any other bit, whatever else it
    /// carries.
    let screen (flavour : SimulatedUnixFlavour) (flags : int) : AttributeChangeScreen =
        // Measured by `at-dirfd.c` (FLAGS, FLAGORDER): every single bit with
        // AT_FDCWD and a regular file, and a rejected bit against an
        // unreadable path, a bad dirfd, an empty path and a missing name,
        // which it beats on both flavours. `chmod-chown-at.c` (FLAGMIX): a
        // rejected bit beside each accepted one is EINVAL too.
        let accepted, unmodelled, noFollow, emptyPath =
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                linuxAtSymlinkNoFollow ||| linuxAtEmptyPath, 0, linuxAtSymlinkNoFollow, linuxAtEmptyPath
            | SimulatedUnixFlavour.Darwin ->
                darwinAtSymlinkNoFollow ||| darwinUnmodelledFlags, darwinUnmodelledFlags, darwinAtSymlinkNoFollow, 0

        if flags &&& ~~~accepted <> 0 then
            AttributeChangeScreen.Failed UnixError.EINVAL
        elif flags &&& unmodelled <> 0 then
            AttributeChangeScreen.Unmodelled flags
        else
            AttributeChangeScreen.Screened
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
