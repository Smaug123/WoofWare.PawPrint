namespace WoofWare.PosixKernel

/// A `struct timespec` as the caller's memory held it: `tv_sec` and
/// `tv_nsec`, both raw, before this kernel decides what they say.
[<Struct>]
type TimespecFields =
    {
        /// `tv_sec`.
        Seconds : int64
        /// `tv_nsec`, which may be any value the caller stored, the flavour's
        /// `UTIME_NOW` and `UTIME_OMIT` among them.
        Nanoseconds : int64
    }

/// The `times` argument of `utimensat(2)`, as the caller found it.
[<RequireQualifiedAccess>]
type TimesArgument =
    /// The null pointer: both times are now.
    | Null
    /// A pointer to memory the caller could not read.
    | Unreadable
    /// The two `struct timespec`s the pointer names: the access time, then the
    /// modification time.
    | Fields of access : TimespecFields * modification : TimespecFields

/// A pathname argument for a call that reads the null pointer differently
/// from any other address it cannot read: `utimensat(2)`'s.
[<RequireQualifiedAccess>]
type NullablePathArgument =
    /// The null pointer.
    | Null
    /// Any other pointer, and what the caller found there.
    | NotNull of PathArgumentBytes

/// What one of `utimensat(2)`'s two times asks for, as its flavour reads the
/// `struct timespec`.
[<RequireQualifiedAccess>]
type TimestampRequest =
    /// `UTIME_NOW`: the time now.
    | Now
    /// `UTIME_OMIT`: leave this time as it is.
    | Omit
    /// This time. On Linux the nanoseconds are within `[0, 1e9)`; Darwin
    /// accepts any, and see `TimestampRange.Nanoseconds64` for what it makes
    /// of them.
    | Explicit of TimespecFields
    /// A nanosecond field the flavour answers EINVAL for, once the call has
    /// found the object whose times it would set. Linux only.
    | Invalid of TimespecFields

/// The flag word of `utimensat(2)`, once this kernel has screened it.
type TimestampChangeArguments =
    {
        /// Whether a symbolic link in the final position is followed:
        /// `AT_SYMLINK_NOFOLLOW` says not.
        FinalSymlink : SymlinkPolicy
        /// What the call makes of an empty path: Linux's `AT_EMPTY_PATH` sets
        /// the times of the object `dirfd` names.
        EmptyPath : EmptyPathMeaning
    }

/// What screening the flag word of `utimensat(2)` came to.
[<RequireQualifiedAccess>]
type TimestampChangeScreen =
    /// The word is one this kernel accepts, and this is what it says.
    | Screened of TimestampChangeArguments
    /// The call fails with this errno before its path is copied in.
    | Failed of error : UnixError
    /// The word carries flags the flavour reads and this library does not
    /// model: Darwin's `AT_SYMLINK_NOFOLLOW_ANY` (0x800), `AT_RESOLVE_BENEATH`
    /// (0x2000) and `AT_UNIQUE` (0x8000). `flags` is the whole word.
    | Unmodelled of flags : int

/// The range of times a filesystem stores, and what it stores for a time
/// outside it.
[<RequireQualifiedAccess>]
type TimestampRange =
    /// Any `tv_sec`, with the nanoseconds as given except at either end of
    /// the 64-bit range, where they are dropped: Linux's tmpfs, devtmpfs and
    /// pipes.
    | Seconds64
    /// The signed 64-bit count of nanoseconds `tv_sec * 1e9 + tv_nsec` names,
    /// computed exactly and saturating at either end: APFS. A nanosecond
    /// field outside `[0, 1e9)` is carried into the seconds, so `(5, 1e9)` is
    /// 6 and `(5, -3)` is three nanoseconds before 5.
    | Nanoseconds64

/// Whether a caller may set an object's times as `utimensat(2)` asks.
[<RequireQualifiedAccess>]
type TimestampChangePermission =
    /// It may.
    | Permitted
    /// It may not, and the call fails with this errno, changing nothing.
    | Denied of error : UnixError
    /// Darwin, a privileged caller, and an object it does not own: unmeasured.
    | UnmeasuredPrivilegedCaller
    /// Darwin, a symbolic link the caller does not own but may write, and
    /// both times now: unmeasured.
    | UnmeasuredSymlinkWrite

[<RequireQualifiedAccess>]
module TimestampChangeRules =

    // Measured by `utimensat-rules.c` (CONST) on Linux 6.18.5 and Darwin 27.0;
    // `<fcntl.h>`'s flag numbering by `at-dirfd.c` (CONST).
    let private linuxUtimeNow : int64 = 0x3FFFFFFFL
    let private linuxUtimeOmit : int64 = 0x3FFFFFFEL
    let private darwinUtimeNow : int64 = -1L
    let private darwinUtimeOmit : int64 = -2L
    let private linuxAtSymlinkNoFollow : int = 0x100
    let private linuxAtEmptyPath : int = 0x1000
    let private darwinAtSymlinkNoFollow : int = 0x20
    // AT_SYMLINK_NOFOLLOW_ANY, AT_RESOLVE_BENEATH and AT_UNIQUE.
    let private darwinUnmodelledFlags : int = 0x800 ||| 0x2000 ||| 0x8000

    let private nanosecondsPerSecond : int64 = 1_000_000_000L

    /// `UTIME_NOW` in `flavour`'s numbering: 0x3FFFFFFF on Linux, -1 on
    /// Darwin.
    let utimeNow (flavour : SimulatedUnixFlavour) : int64 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxUtimeNow
        | SimulatedUnixFlavour.Darwin -> darwinUtimeNow

    /// `UTIME_OMIT` in `flavour`'s numbering: 0x3FFFFFFE on Linux, -2 on
    /// Darwin.
    let utimeOmit (flavour : SimulatedUnixFlavour) : int64 =
        match flavour with
        | SimulatedUnixFlavour.Linux -> linuxUtimeOmit
        | SimulatedUnixFlavour.Darwin -> darwinUtimeOmit

    /// What one `struct timespec` of `utimensat(2)`'s times asks for under
    /// `flavour`: `UTIME_NOW`, `UTIME_OMIT`, or a time. Linux answers EINVAL
    /// for any other nanosecond field outside `[0, 1e9)`; Darwin reads every
    /// other value as a time.
    let decode (flavour : SimulatedUnixFlavour) (fields : TimespecFields) : TimestampRequest =
        // Measured by `utimensat-rules.c` (NSEC): Linux accepts 0 to 999999999
        // and its two markers, and answers EINVAL for 1e9, 1073741821,
        // 1073741824, -1, -2, -3 and the 32- and 64-bit extremes; Darwin
        // accepts every one of those, -1 and -2 as its markers.
        if fields.Nanoseconds = utimeNow flavour then
            TimestampRequest.Now
        elif fields.Nanoseconds = utimeOmit flavour then
            TimestampRequest.Omit
        else

        match flavour with
        | SimulatedUnixFlavour.Darwin -> TimestampRequest.Explicit fields
        | SimulatedUnixFlavour.Linux ->
            if fields.Nanoseconds < 0L || fields.Nanoseconds >= nanosecondsPerSecond then
                TimestampRequest.Invalid fields
            else
                TimestampRequest.Explicit fields

    /// Screen the raw flag word of `utimensat(2)` as `flavour` does.
    ///
    /// Linux accepts `AT_SYMLINK_NOFOLLOW` (0x100) and `AT_EMPTY_PATH`
    /// (0x1000), and answers EINVAL for a word carrying any other bit, before
    /// the path is copied in and before `dirfd` is looked at. Darwin rejects
    /// no bit: it reads `AT_SYMLINK_NOFOLLOW` (0x20) and three flags this
    /// library does not model (see `TimestampChangeScreen.Unmodelled`), and
    /// ignores every other.
    let screen (flavour : SimulatedUnixFlavour) (flags : int) : TimestampChangeScreen =
        // Measured by `at-dirfd.c` (FLAGS, FLAGORDER): each single bit with
        // AT_FDCWD and a regular file; Linux's rejected bit beats an
        // unreadable path, a bad dirfd, an empty path and a missing name.
        // `utimensat-rules.c` (DFLAGS): on Darwin every single bit against a
        // final link, a link mid-path, a path through "..", and an absolute
        // path; only 0x20 (the final link not followed), 0x800 (ELOOP for a
        // link anywhere, the final one not followed) and 0x2000 (an absolute
        // path refused) changed anything.
        match flavour with
        | SimulatedUnixFlavour.Linux ->
            if flags &&& ~~~(linuxAtSymlinkNoFollow ||| linuxAtEmptyPath) <> 0 then
                TimestampChangeScreen.Failed UnixError.EINVAL
            else
                TimestampChangeScreen.Screened
                    {
                        FinalSymlink =
                            if flags &&& linuxAtSymlinkNoFollow <> 0 then
                                SymlinkPolicy.NoFollowFinal
                            else
                                SymlinkPolicy.Follow
                        EmptyPath =
                            if flags &&& linuxAtEmptyPath <> 0 then
                                EmptyPathMeaning.NamesStartingPoint
                            else
                                EmptyPathMeaning.Walked
                    }
        | SimulatedUnixFlavour.Darwin ->
            if flags &&& darwinUnmodelledFlags <> 0 then
                TimestampChangeScreen.Unmodelled flags
            else
                TimestampChangeScreen.Screened
                    {
                        FinalSymlink =
                            if flags &&& darwinAtSymlinkNoFollow <> 0 then
                                SymlinkPolicy.NoFollowFinal
                            else
                                SymlinkPolicy.Follow
                        EmptyPath = EmptyPathMeaning.Walked
                    }

    /// The range a file on a mount of `fileSystem` keeps its times in, or
    /// `None` where that is unmeasured (NFS, whose server decides).
    let rangeOf (fileSystem : EmulatedFileSystemType) : TimestampRange option =
        // Measured by `utimensat-rules.c` (NSEC on /dev/shm, NSECX): tmpfs,
        // devtmpfs and a pipe each keep -2^63 to 2^63 - 1 seconds, every
        // nanosecond field within [0, 1e9) but at those two ends, where it is
        // 0. (ext4, which this library does not model, clamps to -2^31 and
        // 2^34 + 2^31 - 1, dropping the nanoseconds there.) On APFS (NSEC on
        // Darwin), every pair of fields within or beyond the 64-bit count of
        // nanoseconds.
        match fileSystem with
        | EmulatedFileSystemType.Tmpfs -> Some TimestampRange.Seconds64
        | EmulatedFileSystemType.Apfs -> Some TimestampRange.Nanoseconds64
        | EmulatedFileSystemType.Nfs -> None

    /// The time a file whose times are kept in `range` stores for `fields`.
    ///
    /// `fields` is an explicit time as `decode` leaves it: under `Seconds64`
    /// its nanoseconds must lie in `[0, 1e9)`, as Linux's decoding ensures.
    let stored (range : TimestampRange) (fields : TimespecFields) : UnixTimestamp =
        match range with
        | TimestampRange.Seconds64 ->
            if fields.Nanoseconds < 0L || fields.Nanoseconds >= nanosecondsPerSecond then
                failwith
                    $"TimestampChangeRules.stored: %d{fields.Nanoseconds} is not a nanosecond field Linux's decoding lets through; decode first (this is a bug in this library)."

            if fields.Seconds = System.Int64.MaxValue || fields.Seconds = System.Int64.MinValue then
                UnixTimestamp.ofSeconds fields.Seconds
            else
                UnixTimestamp.createOrFail "TimestampChangeRules.stored" fields.Seconds (int fields.Nanoseconds)
        | TimestampRange.Nanoseconds64 ->
            let exact =
                System.Int128.op_Implicit fields.Seconds
                * System.Int128.op_Implicit nanosecondsPerSecond
                + System.Int128.op_Implicit fields.Nanoseconds

            let saturated =
                if exact > System.Int128.op_Implicit System.Int64.MaxValue then
                    System.Int64.MaxValue
                elif exact < System.Int128.op_Implicit System.Int64.MinValue then
                    System.Int64.MinValue
                else
                    int64 exact

            // Floor division, so that the nanosecond part is never negative.
            let quotient = saturated / nanosecondsPerSecond
            let remainder = saturated % nanosecondsPerSecond

            if remainder >= 0L then
                UnixTimestamp.createOrFail "TimestampChangeRules.stored" quotient (int remainder)
            else
                UnixTimestamp.createOrFail
                    "TimestampChangeRules.stored"
                    (quotient - 1L)
                    (int (remainder + nanosecondsPerSecond))

    let private bothNow (access : TimestampRequest) (modification : TimestampRequest) : bool =
        match access, modification with
        | TimestampRequest.Now, TimestampRequest.Now -> true
        | _ -> false

    /// Whether a caller standing as `standing` towards an object with
    /// permission bits `bits` may set its times as `access` and
    /// `modification` ask, under `flavour`. `isSymlink` says the object is a
    /// symbolic link the call did not follow.
    ///
    /// On both flavours the owner may set any times, and a caller who may
    /// write the object may set both to now. Linux lets a privileged caller
    /// do anything, and answers EACCES where only both times now were asked
    /// and EPERM otherwise. Darwin answers EACCES, or EPERM for a symbolic
    /// link, and asks for no permission at all when both times are
    /// `UTIME_OMIT`.
    ///
    /// Neither time may be `TimestampRequest.Invalid`, which the call answers
    /// before it asks this.
    let permission
        (flavour : SimulatedUnixFlavour)
        (standing : Standing)
        (bits : PermissionBits)
        (isSymlink : bool)
        (access : TimestampRequest)
        (modification : TimestampRequest)
        : TimestampChangePermission
        =
        // Measured by `utimensat-rules.c` (EFFECT, NULLFD, EMPTY): Linux, as
        // uid 1000, sets both times now on uid 2000's 0666 file, on its 0664
        // file in the caller's group, on its symbolic link (0777) and on
        // root's /dev/null (0666), and answers EACCES for its 0644 file and
        // for root's pipe (0600); every other times argument is EPERM on each
        // of those, and root may do anything. Darwin, as uid 501: both times
        // now on root's /Users/Shared (01777), EACCES for every other times
        // argument there and for any on root's /private/etc/hosts (0644);
        // EPERM for root's link /tmp (0755) with AT_SYMLINK_NOFOLLOW; and
        // with both times omitted, success on each of those, changing
        // nothing. The owner's own 0444 file takes any times on both.
        match access, modification with
        | TimestampRequest.Invalid _, _
        | _, TimestampRequest.Invalid _ ->
            failwith
                "TimestampChangeRules.permission: a time with an invalid nanosecond field reached the permission check, which the call answers EINVAL before (this is a bug in this library)."
        | _ ->

        let mayWrite = not (PermissionBits.deniedTo standing AccessRequest.Write bits)
        let privileged = standing.Privilege = CallerPrivilege.Privileged

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            if bothNow access modification then
                if standing.Owns || mayWrite then
                    TimestampChangePermission.Permitted
                else
                    TimestampChangePermission.Denied UnixError.EACCES
            elif standing.Owns || privileged then
                TimestampChangePermission.Permitted
            else
                TimestampChangePermission.Denied UnixError.EPERM
        | SimulatedUnixFlavour.Darwin ->
            match access, modification with
            | TimestampRequest.Omit, TimestampRequest.Omit -> TimestampChangePermission.Permitted
            | _ ->

            if standing.Owns then
                TimestampChangePermission.Permitted
            elif privileged then
                TimestampChangePermission.UnmeasuredPrivilegedCaller
            elif isSymlink then
                if bothNow access modification && mayWrite then
                    TimestampChangePermission.UnmeasuredSymlinkWrite
                else
                    TimestampChangePermission.Denied UnixError.EPERM
            elif bothNow access modification && mayWrite then
                TimestampChangePermission.Permitted
            else
                TimestampChangePermission.Denied UnixError.EACCES

    /// The timestamps an object holding `times` has once `utimensat(2)`
    /// under `flavour` has set them as `access` and `modification` ask,
    /// storing an explicit time as `range` does. `now` is the kernel's
    /// realtime clock, and `asNow` what `UTIME_NOW` reads as: that clock on
    /// Linux, and on Darwin, whose libc reads it, that clock in whole
    /// microseconds.
    ///
    /// Linux moves the status-change time whatever was asked, and never the
    /// birth time. Darwin moves the status-change time only with the
    /// modification time, and a modification time earlier than the birth
    /// time pulls the birth time back to it, unless it is before the epoch.
    ///
    /// Neither time may be `TimestampRequest.Invalid`.
    let changed
        (flavour : SimulatedUnixFlavour)
        (range : TimestampRange)
        (now : UnixTimestamp)
        (asNow : UnixTimestamp)
        (access : TimestampRequest)
        (modification : TimestampRequest)
        (times : InodeTimes)
        : InodeTimes
        =
        // Measured by `utimensat-rules.c` (EFFECT, NSEC): on Linux every
        // times argument but both omitted moves ctime; on Darwin, NOW/OMIT,
        // X/OMIT and E/OMIT move the access time alone, and OMIT/E, E/E and
        // a modification time of 0 or 1 second pull the birth time back to
        // the modification time, while one of -1 second, -0.85 or -0.15 does
        // not. `copy-file-syscalls.c` (FUTIMENS-BIRTH): Linux's birth time
        // stays put.
        let resolved (request : TimestampRequest) (current : UnixTimestamp) : UnixTimestamp =
            match request with
            | TimestampRequest.Now -> asNow
            | TimestampRequest.Omit -> current
            | TimestampRequest.Explicit fields -> stored range fields
            | TimestampRequest.Invalid fields ->
                failwith
                    $"TimestampChangeRules.changed: a time with nanosecond field %d{fields.Nanoseconds}, which the call answers EINVAL for, reached the change (this is a bug in this library)."

        let newAccess = resolved access times.Access
        let newModification = resolved modification times.Modification

        let modificationSet =
            match modification with
            | TimestampRequest.Omit -> false
            | _ -> true

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            { times with
                Access = newAccess
                Modification = newModification
                StatusChange = now
            }
        | SimulatedUnixFlavour.Darwin ->
            let pullsBirthBack =
                modificationSet
                && newModification < times.Birth
                && UnixTimestamp.seconds newModification >= 0L

            {
                Access = newAccess
                Modification = newModification
                StatusChange = if modificationSet then now else times.StatusChange
                Birth = if pullsBirthBack then newModification else times.Birth
            }
