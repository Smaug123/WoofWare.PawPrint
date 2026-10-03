namespace WoofWare.PosixKernel

/// The `gid_t`s of the list a `setgroups(2)` names, as far as the caller could
/// read them.
///
/// The words are raw: `(gid_t)-1`, which no process can hold, is one of the
/// things the kernel checks them for.
[<RequireQualifiedAccess>]
type GroupListWords =
    /// Every word of the list: exactly as many as the call's size says.
    | Readable of words : uint32 list
    /// The words before the first one the caller could not read, in order,
    /// and fewer than the call's size says. The kernel faults where it reaches
    /// the next.
    ///
    /// `FaultsAfter []` is also the argument for a list the caller did not
    /// read because its size is negative or above the platform's
    /// `SimulatedUnixPlatform.supplementaryGroupLimit`: no kernel copies such a
    /// list, so what it holds never matters.
    | FaultsAfter of readable : uint32 list

/// What `getresuid(2)` or `getresgid(2)` does with the caller's three buffers.
[<RequireQualifiedAccess>]
type GetIdsAnswer<'Id> =
    /// The call writes the real, effective and saved IDs to the three buffers,
    /// in that order, and returns 0.
    | Copied of real : 'Id * effective : 'Id * saved : 'Id
    /// The call writes `written` to the buffers before the first one it could
    /// not write, in order (the real ID first), and fails with `EFAULT`.
    | Faulted of written : 'Id list

/// Why this kernel will not answer a `setresuid(2)` or `setresgid(2)`.
[<RequireQualifiedAccess>]
type SetIdsRefusal =
    /// This flavour has no such call: Darwin declares neither `setresuid` nor
    /// `setresgid`.
    | NoSuchCall of flavour : SimulatedUnixFlavour
    /// The call would change the effective ID of a process that writes core
    /// dumps.
    ///
    /// Linux then makes the process dumpable or not according to the
    /// `fs.suid_dumpable` sysctl, which this kernel does not model, so whether
    /// the process still writes a core dump is unknown.
    | DumpabilityUnmodelled

[<RequireQualifiedAccess>]
module SetIdsRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which IDs.
    let describe (refusal : SetIdsRefusal) : string =
        match refusal with
        | SetIdsRefusal.NoSuchCall flavour ->
            $"%O{flavour} has no setresuid(2) or setresgid(2); its C library declares neither."
        | SetIdsRefusal.DumpabilityUnmodelled ->
            "the call changes the effective ID of a process that writes core dumps. Linux then sets the process's dumpable flag from the fs.suid_dumpable sysctl, which this kernel does not model, so whether the process would still write a core dump is unknown."

/// Why this kernel will not answer a `setgroups(2)`.
[<RequireQualifiedAccess>]
type SetGroupsRefusal =
    /// A privileged process on this flavour, whose `setgroups` has not been
    /// measured, because measuring it needs root.
    | UnmeasuredPrivileged of flavour : SimulatedUnixFlavour

[<RequireQualifiedAccess>]
module SetGroupsRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and with which list.
    let describe (refusal : SetGroupsRefusal) : string =
        match refusal with
        | SetGroupsRefusal.UnmeasuredPrivileged flavour ->
            $"the process is privileged, and what %O{flavour}'s setgroups(2) does for a privileged process has not been measured: measuring it needs root."

/// Why this kernel will not answer a `getresuid(2)` or `getresgid(2)`.
[<RequireQualifiedAccess>]
type GetIdsRefusal =
    /// This flavour has no such call: Darwin declares neither `getresuid` nor
    /// `getresgid`.
    | NoSuchCall of flavour : SimulatedUnixFlavour
    /// A buffer has no answer at the write that reaches it.
    | Buffer of BufferRefusal

[<RequireQualifiedAccess>]
module GetIdsRefusal =
    /// What this kernel knows about why it will not answer. A client adds which
    /// entry point asked, and what the buffers were.
    let describe (refusal : GetIdsRefusal) : string =
        match refusal with
        | GetIdsRefusal.NoSuchCall flavour ->
            $"%O{flavour} has no getresuid(2) or getresgid(2); its C library declares neither."
        | GetIdsRefusal.Buffer refusal -> BufferRefusal.describe refusal

/// The calls by which a process reads and changes who it is: `getresuid(2)`,
/// `getresgid(2)`, `setresuid(2)`, `setresgid(2)` and `setgroups(2)`.
///
/// Each changes the whole process, every task alike, as the C library's
/// wrappers do. (Linux's own system calls change only the calling thread, and
/// the C library makes every other thread repeat the call.)
[<RequireQualifiedAccess>]
module UnixCredentials =

    // The three IDs of a `getresuid` or `getresgid`, written one buffer at a
    // time. Measured on Linux 6.18.5 (`setresid.c`): with any one of the three
    // pointers NULL the call is EFAULT, and the IDs before it have been
    // written and those after it have not.
    let private getIds<'Id, 'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (real : UserBuffer)
        (effective : UserBuffer)
        (saved : UserBuffer)
        ((realId, effectiveId, savedId) : 'Id * 'Id * 'Id)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetIdsAnswer<'Id>, GetIdsRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (GetIdsRefusal.NoSuchCall SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let rec write
            (written : 'Id list)
            (remaining : (UserBuffer * 'Id) list)
            : Result<GetIdsAnswer<'Id>, GetIdsRefusal>
            =
            match remaining with
            | [] -> Ok (GetIdsAnswer.Copied (realId, effectiveId, savedId))
            | (buffer, id) :: rest ->
                // Each buffer is decided by the write that reaches it: real
                // storage takes its ID, and an unmapped address faults there.
                match buffer with
                | UserBuffer.Mapped -> write (id :: written) rest
                | UserBuffer.Unmapped _ -> Ok (GetIdsAnswer.Faulted (List.rev written))
                | UserBuffer.Opaque -> Error (GetIdsRefusal.Buffer BufferRefusal.OpaqueAtTransfer)
                | UserBuffer.Addressless -> Error (GetIdsRefusal.Buffer BufferRefusal.AddresslessAtTransfer)

        write [] [ real, realId ; effective, effectiveId ; saved, savedId ]

    /// `getresuid(2)`: the process's real, effective and saved user IDs, into
    /// three buffers.
    ///
    /// Changes nothing, so it returns no system. Refused on Darwin, which has
    /// no such call.
    let getresuid<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (real : UserBuffer)
        (effective : UserBuffer)
        (saved : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetIdsAnswer<UserId>, GetIdsRefusal>
        =
        let credentials = system.Process.Credentials

        getIds real effective saved (credentials.RealUser, credentials.EffectiveUser, credentials.SavedUser) system

    /// `getresgid(2)`: the process's real, effective and saved group IDs, into
    /// three buffers.
    ///
    /// Changes nothing, so it returns no system. Refused on Darwin, which has
    /// no such call.
    let getresgid<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (real : UserBuffer)
        (effective : UserBuffer)
        (saved : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<GetIdsAnswer<GroupId>, GetIdsRefusal>
        =
        let credentials = system.Process.Credentials

        getIds real effective saved (credentials.RealGroup, credentials.EffectiveGroup, credentials.SavedGroup) system

    // Linux's rule for `setresuid` and `setresgid`, which differ only in which
    // triple they replace: a privileged process may set each ID to anything,
    // and any other process only to one of the three it already holds. On
    // success, `-1` keeps an ID and every other request replaces it.
    //
    // Measured on Linux 6.18.5 (`setresid.c`), as the whole product of every
    // starting triple over {0, 1000, 1001} and every requested triple over
    // {-1, 0, 1000, 1001, 1002}, from three starting user triples for the
    // groups. "Privileged" is an effective user ID of 0, for the group IDs as
    // for the user IDs (`Credentials.privilege`): an effective group of 0
    // grants nothing, nor does a real or saved user ID of 0, and a process
    // holding a supplementary group may not take it as a real, effective or
    // saved group. In every row the filesystem uid and gid, which this model
    // does not keep apart, followed the effective ones.
    let private replaceIds<'Id when 'Id : equality>
        (privilege : CallerPrivilege)
        ((real, effective, saved) : 'Id * 'Id * 'Id)
        ((askedReal, askedEffective, askedSaved) : 'Id option * 'Id option * 'Id option)
        : ('Id * 'Id * 'Id) option
        =
        let held = [ real ; effective ; saved ]

        let permitted (asked : 'Id option) : bool =
            match privilege, asked with
            | _, None
            | CallerPrivilege.Privileged, Some _ -> true
            | CallerPrivilege.Unprivileged, Some id -> List.contains id held

        if permitted askedReal && permitted askedEffective && permitted askedSaved then
            Some (
                Option.defaultValue real askedReal,
                Option.defaultValue effective askedEffective,
                Option.defaultValue saved askedSaved
            )
        else
            None

    // What a successful change of IDs does to whether the process dumps core.
    //
    // Measured on Linux 6.18.5 (`setresid.c`): the process's dumpable flag
    // was cleared by exactly those successful calls that changed its effective
    // user or group ID, with `fs.suid_dumpable` at its default of 0. Linux sets
    // the flag from that sysctl, which this kernel does not model, so a process
    // that writes core dumps is refused. One that writes none goes on writing
    // none whatever the sysctl says: `CoreDumps.Suppressed` is a suppression
    // the dumpable flag does not decide (the process started dumpable, and a
    // flag set again by a later change cannot lift an `RLIMIT_CORE` of 0).
    let private keepCoreDumps<'Id, 'Task, 'Handler when 'Id : equality and 'Task : comparison and 'Handler : equality>
        (effectiveBefore : 'Id)
        (effectiveAfter : 'Id)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<unit, SetIdsRefusal>
        =
        match system.Process.CoreDumps with
        | CoreDumps.Written when effectiveBefore <> effectiveAfter -> Error SetIdsRefusal.DumpabilityUnmodelled
        | CoreDumps.Written
        | CoreDumps.Suppressed -> Ok ()

    /// `setresuid(2)`: set the process's real, effective and saved user IDs.
    /// `None` leaves an ID as it is.
    ///
    /// A privileged process may set them to anything. Any other process may
    /// only set each to one of the three it already holds, and is otherwise
    /// answered `EPERM` with nothing changed.
    ///
    /// Refused on Darwin, which has no such call, and for a process that writes
    /// core dumps when its effective user ID would change (see
    /// `SetIdsRefusal.DumpabilityUnmodelled`).
    let setresuid<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (real : UserId option)
        (effective : UserId option)
        (saved : UserId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SetIdsRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (SetIdsRefusal.NoSuchCall SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let credentials = system.Process.Credentials

        match
            replaceIds
                (Credentials.privilege credentials)
                (credentials.RealUser, credentials.EffectiveUser, credentials.SavedUser)
                (real, effective, saved)
        with
        | None -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
        | Some (real, effective, saved) ->

        keepCoreDumps credentials.EffectiveUser effective system
        |> Result.map (fun () ->
            SyscallAnswer.Completed 0L,
            { system with
                Process =
                    { system.Process with
                        Credentials =
                            { credentials with
                                RealUser = real
                                EffectiveUser = effective
                                SavedUser = saved
                            }
                    }
            }
        )

    /// `setresgid(2)`: set the process's real, effective and saved group IDs.
    /// `None` leaves an ID as it is.
    ///
    /// A privileged process may set them to anything. Any other process may
    /// only set each to one of the three it already holds (its supplementary
    /// groups do not count), and is otherwise answered `EPERM` with nothing
    /// changed. Privilege is the effective *user* ID's, as everywhere else.
    ///
    /// Refused on Darwin, which has no such call, and for a process that writes
    /// core dumps when its effective group ID would change (see
    /// `SetIdsRefusal.DumpabilityUnmodelled`).
    let setresgid<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (real : GroupId option)
        (effective : GroupId option)
        (saved : GroupId option)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SetIdsRefusal>
        =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Darwin -> Error (SetIdsRefusal.NoSuchCall SimulatedUnixFlavour.Darwin)
        | SimulatedUnixFlavour.Linux ->

        let credentials = system.Process.Credentials

        match
            replaceIds
                (Credentials.privilege credentials)
                (credentials.RealGroup, credentials.EffectiveGroup, credentials.SavedGroup)
                (real, effective, saved)
        with
        | None -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
        | Some (real, effective, saved) ->

        keepCoreDumps credentials.EffectiveGroup effective system
        |> Result.map (fun () ->
            SyscallAnswer.Completed 0L,
            { system with
                Process =
                    { system.Process with
                        Credentials =
                            { credentials with
                                RealGroup = real
                                EffectiveGroup = effective
                                SavedGroup = saved
                            }
                    }
            }
        )

    // Asserted wherever a kernel copies the list: a caller that read the
    // wrong number of words would otherwise be answered about a list it
    // did not pass.
    let private assertWordsAgree (size : int) (words : GroupListWords) : unit =
        match words with
        | GroupListWords.Readable words when List.length words <> size ->
            failwith
                $"UnixCredentials.setgroups: the list is %d{size} groups long, but the caller read %d{List.length words} words of it; GroupListWords.Readable holds every word of the list."
        | GroupListWords.FaultsAfter readable when List.length readable >= size ->
            failwith
                $"UnixCredentials.setgroups: the list is %d{size} groups long, and the caller read %d{List.length readable} words of it before the fault; GroupListWords.FaultsAfter holds fewer words than the list has."
        | GroupListWords.Readable _
        | GroupListWords.FaultsAfter _ -> ()

    // Linux reads the list one word at a time, and refuses `(gid_t)-1` as it
    // reaches it: measured, a list whose first two words are readable and
    // whose second is -1 is EINVAL, and one whose first two are readable
    // groups is EFAULT. An empty list is never read, so no buffer faults then.
    let private readGroupsLinux (size : int) (words : GroupListWords) : Result<GroupId list, UnixError> =
        if size = 0 then
            Ok []
        else

        assertWordsAgree size words

        let rec parse (parsed : GroupId list) (remaining : uint32 list) : Result<GroupId list, UnixError> =
            match remaining with
            | [] -> Ok (List.rev parsed)
            | word :: rest ->
                match GroupId.parse word with
                | Some group -> parse (group :: parsed) rest
                | None -> Error UnixError.EINVAL

        match words with
        | GroupListWords.Readable words -> parse [] words
        | GroupListWords.FaultsAfter readable -> parse [] readable |> Result.bind (fun _ -> Error UnixError.EFAULT)

    /// `setgroups(2)`: replace the process's supplementary groups with the
    /// `size` groups `words` holds, kept in the order given.
    ///
    /// Only a privileged process may. A list longer than the platform's
    /// `SimulatedUnixPlatform.supplementaryGroupLimit`, or of negative size, is
    /// `EINVAL`; an unreadable word is `EFAULT`, and `(gid_t)-1`, which no
    /// process can hold, is `EINVAL`. The flavours check these in different
    /// orders.
    ///
    /// Refused for a privileged Darwin process, whose answer has not been
    /// measured.
    let setgroups<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (size : int)
        (words : GroupListWords)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, SetGroupsRefusal>
        =
        let platform = system.Machine.UnixPlatform
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit platform
        let privilege = UnixProcessState.callerPrivilege system.Process
        let sizeAdmitted = size >= 0 && size <= limit

        let installed (groups : GroupId list) =
            SyscallAnswer.Completed 0L,
            { system with
                Process =
                    { system.Process with
                        Credentials =
                            { system.Process.Credentials with
                                SupplementaryGroups = groups
                            }
                    }
            }

        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            // Measured on Linux 6.18.5 (`setresid.c`): the privilege check comes
            // first, so an unprivileged process is EPERM whatever it passes;
            // then the size, which is EINVAL whatever the buffer; then the
            // words. Changing the groups leaves the dumpable flag alone.
            match privilege with
            | CallerPrivilege.Unprivileged -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
            | CallerPrivilege.Privileged ->

            if not sizeAdmitted then
                Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            else

            match readGroupsLinux size words with
            | Error error -> Ok (SyscallAnswer.Failed error, system)
            | Ok groups -> Ok (installed groups)
        | SimulatedUnixFlavour.Darwin ->
            match privilege with
            | CallerPrivilege.Privileged -> Error (SetGroupsRefusal.UnmeasuredPrivileged SimulatedUnixFlavour.Darwin)
            | CallerPrivilege.Unprivileged ->

            // Measured on Darwin 27.0 at uid 501 (`setresid.c`): the size comes
            // first (as an unsigned count, so a negative one is EINVAL too),
            // then the copy of a non-empty list, whose every fault is EFAULT
            // whatever the words before it held, and only then the privilege
            // check, which is EPERM even for an empty list and for one holding
            // `(gid_t)-1`.
            if not sizeAdmitted then
                Ok (SyscallAnswer.Failed UnixError.EINVAL, system)
            elif size = 0 then
                Ok (SyscallAnswer.Failed UnixError.EPERM, system)
            else

            assertWordsAgree size words

            match words with
            | GroupListWords.FaultsAfter _ -> Ok (SyscallAnswer.Failed UnixError.EFAULT, system)
            | GroupListWords.Readable _ -> Ok (SyscallAnswer.Failed UnixError.EPERM, system)
