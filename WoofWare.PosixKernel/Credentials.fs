namespace WoofWare.PosixKernel

/// A user ID: a `uid_t` a process can hold.
///
/// Every `uint32` except `(uid_t)-1`, which is not an ID but the "leave this
/// one unchanged" argument of `setresuid(2)` and `chown(2)`.
[<Struct>]
type UserId =
    private
    | UserId of uid : uint32

    override this.ToString () : string =
        match this with
        | UserId uid -> string<uint32> uid

[<RequireQualifiedAccess>]
module UserId =
    // Measured on Linux 6.18.5 as root (`docs/plans/2026-08-23-posix-kernel-extraction/credentials.c`):
    // `setuid((uid_t)-1)` is EINVAL, while `setuid(0xFFFFFFFE)` succeeds. Darwin
    // answers EPERM to an unprivileged caller before it looks at the value, so its
    // own answer is unmeasured; refusing the sentinel there too costs nothing a
    // process could have held on Linux.
    let private unchanged : uint32 = System.UInt32.MaxValue

    /// The superuser.
    let root : UserId = UserId 0u

    /// The `uid_t` a kernel would report for this ID.
    let toUInt32 (uid : UserId) : uint32 =
        match uid with
        | UserId uid -> uid

    /// The user ID `candidate` names, or `None` if it is `(uid_t)-1`.
    let parse (candidate : uint32) : UserId option =
        if candidate = unchanged then
            None
        else
            Some (UserId candidate)

    /// As `parse`, but failing with `context` in the message if `candidate` is
    /// `(uid_t)-1`.
    let parseOrFail (context : string) (candidate : uint32) : UserId =
        match parse candidate with
        | Some uid -> uid
        | None ->
            failwith
                $"%s{context}: %d{candidate} is (uid_t)-1, which is not a user ID a process can hold; setresuid(2) and chown(2) read it as \"leave this ID unchanged\"."

/// A group ID: a `gid_t` a process can hold.
///
/// Every `uint32` except `(gid_t)-1`, which is not an ID but the "leave this
/// one unchanged" argument of `setresgid(2)` and `chown(2)`.
[<Struct>]
type GroupId =
    private
    | GroupId of gid : uint32

    override this.ToString () : string =
        match this with
        | GroupId gid -> string<uint32> gid

[<RequireQualifiedAccess>]
module GroupId =
    // Measured on Linux 6.18.5 as root: `setgid((gid_t)-1)` is EINVAL, and so is
    // `setgroups` with `(gid_t)-1` among the groups. Darwin's answer is unmeasured,
    // as for `UserId`.
    let private unchanged : uint32 = System.UInt32.MaxValue

    /// The `gid_t` a kernel would report for this ID.
    let toUInt32 (gid : GroupId) : uint32 =
        match gid with
        | GroupId gid -> gid

    /// The group ID `candidate` names, or `None` if it is `(gid_t)-1`.
    let parse (candidate : uint32) : GroupId option =
        if candidate = unchanged then
            None
        else
            Some (GroupId candidate)

    /// As `parse`, but failing with `context` in the message if `candidate` is
    /// `(gid_t)-1`.
    let parseOrFail (context : string) (candidate : uint32) : GroupId =
        match parse candidate with
        | Some gid -> gid
        | None ->
            failwith
                $"%s{context}: %d{candidate} is (gid_t)-1, which is not a group ID a process can hold; setresgid(2) and chown(2) read it as \"leave this ID unchanged\"."

/// Who a process is: the IDs a kernel consults when it decides what the
/// process may do.
///
/// The real IDs are who started the process, the effective IDs are who it is
/// acting as, and the saved IDs are what it may switch its effective IDs back
/// to. They differ only in a process that was started from a set-user-ID or
/// set-group-ID executable, or that has changed its IDs since.
type Credentials =
    {
        RealUser : UserId
        EffectiveUser : UserId
        SavedUser : UserId
        RealGroup : GroupId
        EffectiveGroup : GroupId
        SavedGroup : GroupId
        /// The groups `setgroups(2)` was given, in the order it was given
        /// them, duplicates and all.
        ///
        /// Not necessarily what `getgroups(2)` reports: Linux reports these
        /// sorted, and a Darwin process's list usually starts with its
        /// effective group.
        SupplementaryGroups : GroupId list
    }

[<RequireQualifiedAccess>]
module Credentials =
    /// The credentials of a process whose real, effective and saved IDs are all
    /// `user` and `group`, which is every process that has not been started
    /// set-user-ID or set-group-ID and has not changed its IDs since.
    let ofIds (user : UserId) (group : GroupId) (supplementaryGroups : GroupId list) : Credentials =
        {
            RealUser = user
            EffectiveUser = user
            SavedUser = user
            RealGroup = group
            EffectiveGroup = group
            SavedGroup = group
            SupplementaryGroups = supplementaryGroups
        }

    /// Whether a process with these credentials is exempt from the permission
    /// rules a kernel applies to everyone else.
    ///
    /// Decided by the effective user ID alone: effective uid 0, and nothing else.
    let privilege (credentials : Credentials) : CallerPrivilege =
        // Measured on Linux 6.18.5 (`credentials.c`): binding port 80 succeeds with
        // real 1000 / effective 0 / saved 1000, and is EACCES with real 0 /
        // effective 1000 / saved 0 and with real 1000 / effective 1000 / saved 0.
        // Opening a root-owned 0600 file follows the same split. Group 0 is not
        // root.
        //
        // Every privilege question goes through here rather than comparing an ID
        // at its own site, because the sites answer *different* questions from
        // the same fact (whether `open` may ignore a mode that forbids the access
        // it was asked for, whether a write keeps a file's set-user-ID bits,
        // whether `bind` may take a port below 1024) and must not drift apart
        // about who root is.
        if credentials.EffectiveUser = UserId.root then
            CallerPrivilege.Privileged
        else
            CallerPrivilege.Unprivileged
