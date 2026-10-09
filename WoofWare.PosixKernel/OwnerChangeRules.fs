namespace WoofWare.PosixKernel

/// The user a `chown(2)`-family call asks an inode to be owned by, as the
/// kernel's rule reads it: relative to the inode's current owner and to the
/// caller.
[<RequireQualifiedAccess>]
type RequestedUser =
    /// `(uid_t)-1`: leave the owner as it is.
    | Unchanged
    /// The inode's current owner, named explicitly.
    | Current
    /// The caller's effective user, which is not the inode's current owner.
    | Callers
    /// Any other user.
    | Other

/// The group a `chown(2)`-family call asks an inode to have, as the kernel's
/// rule reads it: relative to the inode's current group and to the caller's
/// groups.
[<RequireQualifiedAccess>]
type RequestedGroup =
    /// `(gid_t)-1`: leave the group as it is.
    | Unchanged
    /// The inode's current group, named explicitly, whether or not the caller
    /// is in it.
    | Current
    /// A group the caller is in, by its effective group or a supplementary
    /// group, which is not the inode's current group.
    | CallersGroup
    /// Any other group.
    | Other

/// What a `chown(2)`-family call asks of one inode, as the kernel's rule reads
/// it. The kernel derives it from the IDs the call names, the caller's
/// credentials and the inode's owner.
type OwnerChangeRequest =
    {
        UserAsked : RequestedUser
        GroupAsked : RequestedGroup
    }

/// What kind of inode a `chown(2)`-family call reached, as far as which bits it
/// clears is concerned.
[<RequireQualifiedAccess>]
type internal OwnerChangeTarget =
    | Directory
    /// A regular file, a symbolic link or a pipe.
    | NonDirectory

/// Who may change an inode's owner and group, and which of its set-user-ID and
/// set-group-ID bits a change clears: each flavour's rule for `chown(2)`,
/// `lchown(2)` and `fchown(2)`.
///
/// The flavours disagree about every part of it, so each case is one flavour's
/// whole rule, which `UnixPathResolution.chown` and its relatives apply.
[<RequireQualifiedAccess>]
type OwnerChangeRule =
    /// An unprivileged caller may name a user only if it owns the inode and
    /// names itself, and a group only if it owns the inode and names the
    /// current group or one of its own. A privileged caller may name anything.
    ///
    /// Every success clears bits from anything but a directory, whether or not
    /// it named an ID, and whoever the caller is: `S_ISUID` always, and
    /// `S_ISGID` if the inode is group-executable, or the caller is neither
    /// privileged nor in the inode's group. A directory keeps both. Clearing a
    /// bit is a mode change, which only the owner or a privileged caller may
    /// make, so a non-owner's otherwise permitted call is `EPERM` when it would
    /// clear one.
    ///
    /// Every success moves the inode's `ctime`, even one that names no ID.
    ///
    /// This is Linux.
    | ClearsSetIdFromNonDirectories
    /// An unprivileged caller may name a user only if it is the inode's
    /// current owner, and a group only if it is the current group, or the
    /// caller owns the inode and names one of its own groups. That holds for
    /// a non-owner as for the owner: anyone may name the IDs an inode already
    /// has.
    ///
    /// A call naming either ID clears both `S_ISUID` and `S_ISGID`, from every
    /// kind of inode, and moves its `ctime`. A call naming neither succeeds and
    /// changes nothing, not even `ctime`.
    ///
    /// What a privileged caller may do, and what a call clears where an
    /// ordinary user cannot set up the inode to ask, have not been measured;
    /// `chown` refuses those rows (`OwnerChangeRefusal`).
    ///
    /// This is Darwin.
    | ClearsSetIdWhenAnIdIsNamed

/// What a `chown(2)`-family call does to one inode, for one caller.
[<RequireQualifiedAccess>]
type internal OwnerChange =
    /// The caller may not make this change. The syscall answers `EPERM` and
    /// changes nothing.
    | Forbidden
    /// The syscall succeeds and changes nothing at all, not even `ctime`.
    | Untouched
    /// The syscall succeeds: the inode's owner and group become the IDs the
    /// call named (each unnamed one staying as it was), its permission bits
    /// become `bits`, and its `ctime` moves.
    | Changed of bits : PermissionBits

/// Why this library will not say what a `chown(2)`-family call does: the
/// kernel's answer for that caller and that inode has not been measured.
[<RequireQualifiedAccess>]
type OwnerChangeRefusal =
    /// A privileged caller, under `OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed`,
    /// standing as `standing` towards an inode carrying `bits`, asking
    /// `request`.
    | UnmeasuredPrivilegedCaller of standing : Standing * request : OwnerChangeRequest * bits : PermissionBits
    /// An unprivileged caller, under
    /// `OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed`, standing as `standing`
    /// towards an inode carrying `bits` and asking `request`, where an
    /// ordinary user could not set up the inode to measure: an owner outside
    /// the inode's group, of an inode with `S_ISGID`; a non-owner naming an ID
    /// of an inode with either set-ID bit; or a non-owner asking nothing of an
    /// inode with `S_ISGID`.
    | UnmeasuredSetIdChange of standing : Standing * request : OwnerChangeRequest * bits : PermissionBits

[<RequireQualifiedAccess>]
module OwnerChangeRefusal =
    /// What this library knows about why it will not answer. A client adds
    /// which call it was answering and which inode it was.
    let describe (refusal : OwnerChangeRefusal) : string =
        let measured =
            "What has been measured, as an ordinary user: the owner, in the inode's group or not, over every mode it could set up (an owner outside the group cannot set S_ISGID), naming every kind of user and group; and a non-owner, in the group or not, of files, directories and symbolic links without set-ID bits, and of a set-user-ID file asked to change nothing."

        match refusal with
        | OwnerChangeRefusal.UnmeasuredPrivilegedCaller (standing, request, bits) ->
            $"what Darwin's chown does for a privileged caller, standing %A{standing} towards an inode carrying %O{bits} and asking %A{request}, has not been measured; measuring it needs root. %s{measured}"
        | OwnerChangeRefusal.UnmeasuredSetIdChange (standing, request, bits) ->
            $"what Darwin's chown does for a caller standing %A{standing} towards an inode carrying %O{bits} and asking %A{request} has not been measured; setting that inode up needs root or a second user. %s{measured}"

[<RequireQualifiedAccess>]
module internal OwnerChangeRequest =
    /// How the rule reads a call naming `user` and `group` (`None` for
    /// `(uid_t)-1` and `(gid_t)-1`), made by a process with `credentials` of
    /// an inode owned by `owner`.
    ///
    /// The effective user and the groups `Credentials.isInGroup` counts decide
    /// which IDs are the caller's own; the real and saved IDs play no part.
    let classify
        (credentials : Credentials)
        (owner : InodeOwner)
        (user : UserId option)
        (group : GroupId option)
        : OwnerChangeRequest
        =
        {
            UserAsked =
                match user with
                | None -> RequestedUser.Unchanged
                | Some user when user = owner.User -> RequestedUser.Current
                | Some user when user = credentials.EffectiveUser -> RequestedUser.Callers
                | Some _ -> RequestedUser.Other
            GroupAsked =
                match group with
                | None -> RequestedGroup.Unchanged
                | Some group when group = owner.Group -> RequestedGroup.Current
                | Some group when Credentials.isInGroup credentials group -> RequestedGroup.CallersGroup
                | Some _ -> RequestedGroup.Other
        }

[<RequireQualifiedAccess>]
module internal OwnerChangeRules =

    let private nothingAsked : OwnerChangeRequest =
        {
            UserAsked = RequestedUser.Unchanged
            GroupAsked = RequestedGroup.Unchanged
        }

    let private linux
        (standing : Standing)
        (request : OwnerChangeRequest)
        (target : OwnerChangeTarget)
        (bits : PermissionBits)
        : OwnerChange
        =
        // Measured by `chown-rules.c` on Linux 6.18.5 (ext4 and tmpfs alike),
        // with no mismatch against this rule: every one of the 4096 modes, on
        // a file and on a directory, for the owner in the inode's group by its
        // effective gid and by a supplementary group, outside it, a non-owner
        // in each of those three relations, and root owning the inode or not,
        // in its group or not; each asking every kind of user (unchanged, the
        // current owner, itself, someone else) and group (unchanged, the
        // current one, one of its own, someone else's). Pipes over the same
        // modes and callers, and symbolic links through lchown, answered the
        // same.
        let privileged = standing.Privilege = CallerPrivilege.Privileged

        let userAllowed =
            match request.UserAsked with
            | RequestedUser.Unchanged -> true
            | RequestedUser.Current -> standing.Owns
            | RequestedUser.Callers
            | RequestedUser.Other -> false

        let groupAllowed =
            match request.GroupAsked with
            | RequestedGroup.Unchanged -> true
            | RequestedGroup.Current
            | RequestedGroup.CallersGroup -> standing.Owns
            | RequestedGroup.Other -> false

        if not privileged && not (userAllowed && groupAllowed) then
            OwnerChange.Forbidden
        else

        let raw = PermissionBits.toInt bits

        let cleared =
            match target with
            | OwnerChangeTarget.Directory -> 0
            | OwnerChangeTarget.NonDirectory ->
                let clearsGroup =
                    raw &&& PermissionBits.groupExecute <> 0 || not (standing.InGroup || privileged)

                raw
                &&& (PermissionBits.setUserId
                     ||| (if clearsGroup then PermissionBits.setGroupId else 0))

        // Clearing a bit is a mode change, which the owner and a privileged
        // caller may make and nobody else may: measured, a non-owner's
        // chown(-1, -1) succeeds on a 0644 file and is EPERM on a 02644 one
        // outside its group.
        if cleared <> 0 && not standing.Owns && not privileged then
            OwnerChange.Forbidden
        else
            OwnerChange.Changed (PermissionBits.parseOrFail "OwnerChangeRules.verdict" (raw &&& ~~~cleared))

    // Darwin's measurements are an ordinary user's: it can set up any mode on
    // an inode it owns in one of its groups, only modes without `S_ISGID` on
    // one it owns outside them, and nothing at all on another user's inode,
    // whose mode it can only find. Among other users' inodes it found none
    // with `S_ISGID`, and one with `S_ISUID` it could ask only to change
    // nothing, since anything else would change it if the kernel allowed it.
    let private darwinMeasured (standing : Standing) (request : OwnerChangeRequest) (bits : PermissionBits) : bool =
        let raw = PermissionBits.toInt bits

        raw &&& (PermissionBits.setUserId ||| PermissionBits.setGroupId) = 0
        || standing.Owns && (standing.InGroup || raw &&& PermissionBits.setGroupId = 0)
        || not standing.Owns
           && request = nothingAsked
           && raw &&& PermissionBits.setGroupId = 0

    let private darwin
        (standing : Standing)
        (request : OwnerChangeRequest)
        (bits : PermissionBits)
        : Result<OwnerChange, OwnerChangeRefusal>
        =
        // Measured by `chown-rules.c` on Darwin 27.0 at uid 501, with no
        // mismatch against this rule: every mode the owner could set, on a
        // file and on a directory, in its effective group, in a supplementary
        // group, and outside its groups, each asking every kind of user and
        // group; and other users' files, directories and symbolic links
        // without set-ID bits, in its group and outside it, asked for nothing,
        // for their current IDs, for itself, for one of its groups, and for
        // other IDs. A call that names an ID clears both set-ID bits whether
        // or not either ID changes, on a directory as on a file: `06745`
        // becomes `00745` under chown(501, -1) by its owner.
        match standing.Privilege with
        | CallerPrivilege.Privileged -> Error (OwnerChangeRefusal.UnmeasuredPrivilegedCaller (standing, request, bits))
        | CallerPrivilege.Unprivileged ->

        if not (darwinMeasured standing request bits) then
            Error (OwnerChangeRefusal.UnmeasuredSetIdChange (standing, request, bits))
        elif request = nothingAsked then
            Ok OwnerChange.Untouched
        else

        let userAllowed =
            match request.UserAsked with
            | RequestedUser.Unchanged
            | RequestedUser.Current -> true
            | RequestedUser.Callers
            | RequestedUser.Other -> false

        let groupAllowed =
            match request.GroupAsked with
            | RequestedGroup.Unchanged
            | RequestedGroup.Current -> true
            | RequestedGroup.CallersGroup -> standing.Owns
            | RequestedGroup.Other -> false

        if userAllowed && groupAllowed then
            PermissionBits.toInt bits
            &&& ~~~(PermissionBits.setUserId ||| PermissionBits.setGroupId)
            |> PermissionBits.parseOrFail "OwnerChangeRules.verdict"
            |> OwnerChange.Changed
            |> Ok
        else
            Ok OwnerChange.Forbidden

    /// What a `chown(2)`-family call asking `request`, made by a caller
    /// standing as `standing` towards a `target` inode carrying `bits`, does to
    /// that inode under `rule`; see `OwnerChangeRule` for each flavour's rule.
    ///
    /// For a symbolic link, `bits` are the platform's link permissions, which
    /// carry no set-ID bit, so a change never alters them.
    let verdict
        (rule : OwnerChangeRule)
        (standing : Standing)
        (request : OwnerChangeRequest)
        (target : OwnerChangeTarget)
        (bits : PermissionBits)
        : Result<OwnerChange, OwnerChangeRefusal>
        =
        let bits = PermissionBits.assertValid "OwnerChangeRules.verdict" bits

        match rule with
        | OwnerChangeRule.ClearsSetIdFromNonDirectories -> Ok (linux standing request target bits)
        | OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed -> darwin standing request bits
