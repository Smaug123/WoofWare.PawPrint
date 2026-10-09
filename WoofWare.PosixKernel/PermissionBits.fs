namespace WoofWare.PosixKernel

/// <summary>
/// Whether the calling process is exempt from the file-permission rules.
/// </summary>
/// <example>
/// If the emulated user has UID 0, for example, they are exempt.
/// </example>
[<RequireQualifiedAccess>]
type CallerPrivilege =
    /// <summary>The caller is privileged and can ignore file permissions.</summary>
    /// <example>
    /// The Linux root user can write to a <c>04755</c> file (keeping the perms unchanged),
    /// and can search a <c>0o000</c> directory.
    /// </example>
    | Privileged
    /// <summary>
    /// The caller is not privileged so is bound by file permissions.
    /// </summary>
    | Unprivileged

/// <summary>
/// The one thing a caller is asking permission to do, as
/// <c>PermissionBits.deniedTo</c> is asked it.
/// </summary>
/// <remarks>
/// There is deliberately no case for executing anything but a directory, because
/// root is not exempt from that question as it is from these three: Linux grants
/// root <c>X_OK</c> on a non-directory only when at least one of the three execute
/// bits is set, and Darwin's answer for root has not been measured. Ask
/// <c>PermissionBits.executionDenied</c> instead, which takes the platform's
/// rule.
/// </remarks>
[<RequireQualifiedAccess>]
type internal AccessRequest =
    /// <summary>Read the object's contents: <c>R_OK</c>, the <c>0o400</c> bit.</summary>
    | Read
    /// <summary>Change the object's contents: <c>W_OK</c>, the <c>0o200</c> bit.</summary>
    | Write
    /// <summary>
    /// Traverse a directory, or name something inside it: <c>X_OK</c> on a
    /// directory, the <c>0o100</c> bit.
    /// </summary>
    /// <remarks>
    /// Named for the directory because the same bit means something else on a
    /// regular file, where it is "execute". Nothing locally enforces that the caller
    /// really is a directory.
    /// </remarks>
    | SearchDirectory

/// How a caller stands towards one inode: whether it is exempt from the
/// permission rules, whether it owns the inode, and whether it is in the
/// inode's group.
///
/// Every rule that depends on who the caller is reads one of these rather
/// than comparing IDs itself. The kernel derives it from a process's
/// credentials and the inode's owner.
///
/// Any combination of the three is one some caller can be in: root can own an
/// inode or not, and an owner need not be in its inode's group.
type Standing =
    {
        /// Whether the caller is exempt from the permission rules.
        Privilege : CallerPrivilege
        /// Whether the caller's effective user ID is the inode's owner.
        Owns : bool
        /// Whether the caller's effective group ID, or one of its
        /// supplementary groups, is the inode's group.
        InGroup : bool
    }

/// What a directory's sticky bit (`S_ISVTX`) says about removing, renaming or
/// replacing one of its entries.
[<RequireQualifiedAccess>]
type internal StickyRemoval =
    /// The sticky bit forbids nothing: the directory does not carry it, or the
    /// caller owns the directory or the entry.
    | Unrestricted
    /// The directory carries the sticky bit and the caller owns neither the
    /// directory nor the entry, but the caller is privileged.
    | ForbiddenButPrivileged
    /// The directory carries the sticky bit, the caller owns neither the
    /// directory nor the entry, and the caller is not privileged.
    | Forbidden

/// <summary>
/// Whether this Unix clears <c>S_ISGID</c> when an unprivileged process changes a
/// file's contents, on a file that is not group-executable.
/// </summary>
///
/// <remarks>
/// By contrast, both modelled Unixes clear <c>S_ISUID</c> and leave the sticky
/// bit alone during a write from an unprivileged process, so there's no
/// configuration knob for that behaviour.
/// The behaviour is for security purposes: writing to an executable file should
/// not cause you to be able to run your arbitrary new contents as an impersonated
/// user.
/// </remarks>
[<RequireQualifiedAccess>]
type SetGroupIdOnWrite =
    /// <summary>
    /// An unprivileged write to the file clears <c>S_ISGID</c> if the file is
    /// group-executable, or if the writer is not in the file's group.
    /// </summary>
    /// <remarks>
    /// On Linux, <c>S_ISGID</c> without <c>S_IXGRP</c> means "mandatory locking" rather than privilege,
    /// so a write by a member of the file's group can (and does) safely leave it alone.
    /// Whether the writer owns the file makes no difference.
    /// </remarks>
    /// <example>
    /// This is the case on Linux.
    ///
    /// For example, <c>02644</c> remains <c>02644</c> after an unprivileged write by a member of the
    /// file's group, and becomes <c>00644</c> after one by anyone else.
    /// </example>
    | StripWhenGroupExecutableOrWriterOutsideGroup
    /// <summary>
    /// Regardless of what the execute bits say, the group ID from <c>setgid</c> gets
    /// cleared after an unprivileged write.
    /// </summary>
    /// <remarks>
    /// On all platforms, <c>S_ISUID</c> already behaves this way.
    ///
    /// Only a writer who owns the file has been measured, and only a writer in the file's
    /// group when the file is set-group-ID. Asked about a set-user-ID or set-group-ID file
    /// on behalf of any other writer, privileged or not, the write is refused
    /// (<c>SetIdChangeRefusal.UnmeasuredDarwinWrite</c>) rather than guessed at.
    /// </remarks>
    /// <example>
    /// This is the case on Darwin.
    ///
    /// For example, <c>02644</c> becomes <c>00644</c> after an unprivileged write.
    /// </example>
    | StripAlways

/// <summary>
/// Whether this Unix clears a file's set-user-ID (<c>setuid</c>) and set-group-ID (<c>setgid</c>)
/// bits on truncating a file.
/// </summary>
[<RequireQualifiedAccess>]
type SetIdBitsOnTruncation =
    /// <summary>
    /// This Unix clears <c>S_ISUID</c> and <c>S_ISGID</c> on file truncation.
    /// </summary>
    /// <remarks>
    /// That is, truncation is a content change like any other, and it clears the
    /// same bits that a normal write would clear: <c>S_ISUID</c> always, and <c>S_ISGID</c>
    /// if the file is group-executable or the truncating process is not in the file's group.
    /// </remarks>
    /// <example>
    /// Linux behaves this way.
    /// </example>
    | Strip
    /// <summary>
    /// This Unix leaves <c>S_ISUID</c> and <c>S_ISGID</c> alone on file truncation,
    /// even if a write to the same file by the same process would strip them.
    /// </summary>
    /// <remarks>
    /// Only a truncating process that owns the file has been measured, and only one in the
    /// file's group when the file is set-group-ID. Asked about a set-user-ID or set-group-ID
    /// file on behalf of any other process, privileged or not, the truncation is refused
    /// (<c>SetIdChangeRefusal.UnmeasuredDarwinTruncation</c>) rather than guessed at.
    /// </remarks>
    /// <example>
    /// Darwin behaves this way.
    /// </example>
    | Preserve

/// <summary>
/// The permission, set-user-ID, set-group-ID and sticky bits of an inode's
/// mode.
/// </summary>
/// <remarks>
/// This is <c>st_mode &amp; 0o7777</c>.
///
/// You would feed this to <c>chmod(2)</c>, for example.
///
/// Deliberately <i>not</i> the <c>S_IFMT</c> file-type band, which is derived from
/// <c>InodeContent</c> by <c>InodeContent.fileTypeBits</c> instead (because
/// <c>chmod(2)</c> can't set that).
/// </remarks>
[<Struct>]
type PermissionBits =
    private
    | PermissionBits of bits : int

    /// <summary>
    /// The octal digits that you would use in chmod, for example.
    /// </summary>
    /// <example>
    /// "a+rwx,u-x,g-wx,o-wx,ug-s,-t" gives a <c>ToString</c> of "0o0644".
    /// </example>
    override this.ToString () : string =
        match this with
        | PermissionBits bits -> "0o" + System.Convert.ToString(bits, 8).PadLeft (4, '0')

/// Why this library will not say what a write or a truncation does to a file's
/// set-user-ID and set-group-ID bits: the kernel's answer for that caller and
/// that mode has not been measured.
[<RequireQualifiedAccess>]
type SetIdChangeRefusal =
    /// A content-changing write, under `SetGroupIdOnWrite.StripAlways`, by a
    /// caller standing as `standing` towards a file whose bits are `bits`.
    | UnmeasuredDarwinWrite of standing : Standing * bits : PermissionBits
    /// A truncation, under `SetIdBitsOnTruncation.Preserve`, by a caller
    /// standing as `standing` towards a file whose bits are `bits`.
    | UnmeasuredDarwinTruncation of standing : Standing * bits : PermissionBits

[<RequireQualifiedAccess>]
module SetIdChangeRefusal =
    /// What this library knows about why it will not answer. A client adds
    /// which call it was answering and which file it was.
    let describe (refusal : SetIdChangeRefusal) : string =
        let measured =
            "What has been measured: a file with neither set-ID bit, a caller that owns the file and is in its group, and an unprivileged owner outside the group of a file without S_ISGID. Measuring the rest needs root or a second user on Darwin."

        match refusal with
        | SetIdChangeRefusal.UnmeasuredDarwinWrite (standing, bits) ->
            $"what Darwin does to the set-ID bits of a %O{bits} file written to by a caller standing %A{standing} towards it has not been measured. %s{measured}"
        | SetIdChangeRefusal.UnmeasuredDarwinTruncation (standing, bits) ->
            $"what Darwin does to the set-ID bits of a %O{bits} file truncated by a caller standing %A{standing} towards it has not been measured. %s{measured}"

/// What a privileged caller is granted when it asks to execute something that
/// is not a directory: `X_OK` on a regular file or a symbolic link.
///
/// An unprivileged caller's rule is the same on every modelled Unix: it is
/// refused unless the triple its standing selects has the execute bit.
[<RequireQualifiedAccess>]
type PrivilegedExecution =
    /// Granted exactly when at least one of the three execute bits is set,
    /// whichever triple it is in, and whether or not the caller owns the inode
    /// or is in its group.
    ///
    /// This is Linux.
    | NeedsAnExecuteBit
    /// What a privileged caller is granted has not been measured, so a call
    /// that asks is refused (`ExecutionRefusal.UnmeasuredPrivilegedCaller`)
    /// rather than guessed at.
    ///
    /// This is Darwin, where measuring it needs root.
    | Unmeasured

/// Why this library will not say whether a caller may execute something: the
/// kernel's answer for that caller has not been measured.
[<RequireQualifiedAccess>]
type ExecutionRefusal =
    /// A privileged caller, under `PrivilegedExecution.Unmeasured`, standing
    /// as `standing` towards an inode carrying `bits`.
    | UnmeasuredPrivilegedCaller of standing : Standing * bits : PermissionBits

[<RequireQualifiedAccess>]
module ExecutionRefusal =
    /// What this library knows about why it will not answer. A client adds
    /// which call it was answering and which inode it was.
    let describe (refusal : ExecutionRefusal) : string =
        match refusal with
        | ExecutionRefusal.UnmeasuredPrivilegedCaller (standing, bits) ->
            $"whether Darwin lets a privileged caller, standing %A{standing} towards it, execute a non-directory carrying %O{bits} has not been measured. What has been measured: every unprivileged caller's answer, which is the owner, group or other triple's execute bit. Linux grants root execution exactly when some execute bit is set; measuring Darwin's root needs root."

/// Whether reading a symbolic link's target, `readlink(2)`, consults the
/// link's own permission bits.
[<RequireQualifiedAccess>]
type LinkReadRule =
    /// The link's bits are never consulted: a caller that reaches the link
    /// reads it, whatever its mode, owner and group.
    ///
    /// This is Linux, where no syscall gives a link any mode but 0777.
    | ModeIgnored
    /// An unprivileged caller is refused unless the triple its standing
    /// selects has the read bit. What a privileged caller is granted has not
    /// been measured, so a call that asks is refused
    /// (`LinkReadRefusal.UnmeasuredPrivilegedCaller`) rather than guessed at.
    ///
    /// This is Darwin, where measuring the privileged caller needs root.
    | ReadBitOfSelectedTriple

/// Why this library will not say whether a caller may read a symbolic link's
/// target: the kernel's answer for that caller has not been measured.
[<RequireQualifiedAccess>]
type LinkReadRefusal =
    /// A privileged caller, under `LinkReadRule.ReadBitOfSelectedTriple`,
    /// standing as `standing` towards a link carrying `bits`.
    | UnmeasuredPrivilegedCaller of standing : Standing * bits : PermissionBits

[<RequireQualifiedAccess>]
module LinkReadRefusal =
    /// What this library knows about why it will not answer. A client adds
    /// which call it was answering and which link it was.
    let describe (refusal : LinkReadRefusal) : string =
        match refusal with
        | LinkReadRefusal.UnmeasuredPrivilegedCaller (standing, bits) ->
            $"whether Darwin lets a privileged caller, standing %A{standing} towards it, read the target of a symbolic link carrying %O{bits} has not been measured. What has been measured: an unprivileged caller is refused unless the owner, group or other triple its standing selects has the read bit. Measuring Darwin's root needs root."

/// What a privileged caller's `chmod(2)` or `fchmod(2)` does to the mode it
/// asks for.
///
/// An unprivileged caller's rule is the same on every modelled Unix, so it is
/// not a parameter: see `UnixPathResolution.chmod`.
[<RequireQualifiedAccess>]
type PrivilegedModeChange =
    /// The inode gets exactly the twelve bits asked for, whether or not the
    /// caller owns it and whether or not it is in the inode's group.
    ///
    /// This is Linux.
    | SetsRequestedBits
    /// What a privileged caller's mode change does has not been measured, so
    /// the change is refused (`ModeChangeRefusal.UnmeasuredPrivilegedCaller`)
    /// rather than guessed at.
    ///
    /// This is Darwin, where measuring it needs root.
    | Unmeasured

/// What `chmod(2)` or `fchmod(2)` does to one inode's mode, for one caller.
[<RequireQualifiedAccess>]
type internal ModeChange =
    /// The caller may not change this inode's mode: it neither owns the inode
    /// nor is privileged. The syscall answers `EPERM` and changes nothing.
    | Forbidden
    /// The inode's permission bits become `bits`, which need not be the bits
    /// asked for.
    | Permitted of bits : PermissionBits

/// Why this library will not say what a `chmod(2)` or `fchmod(2)` does: the
/// kernel's answer for that caller has not been measured.
[<RequireQualifiedAccess>]
type ModeChangeRefusal =
    /// A privileged caller, under `PrivilegedModeChange.Unmeasured`, standing
    /// as `standing` towards the inode and asking for `requested`.
    | UnmeasuredPrivilegedCaller of standing : Standing * requested : PermissionBits

[<RequireQualifiedAccess>]
module ModeChangeRefusal =
    /// What this library knows about why it will not answer. A client adds
    /// which call it was answering and which inode it was.
    let describe (refusal : ModeChangeRefusal) : string =
        match refusal with
        | ModeChangeRefusal.UnmeasuredPrivilegedCaller (standing, requested) ->
            $"what Darwin's chmod does for a privileged caller, standing %A{standing} towards the inode and asking for %O{requested}, has not been measured. What has been measured: every unprivileged caller, owner or not, in the inode's group or not, over every mode. Measuring the privileged rows needs root on Darwin."

[<RequireQualifiedAccess>]
module PermissionBits =
    /// <summary>
    /// The widest <c>st_mode &amp; 0o7777</c> can be:
    /// three rwx triples, plus setuid, setgid and the sticky bit.
    /// </summary>
    let private widest : int = 0o7777

    // `<sys/stat.h>`'s S_ISUID, S_ISGID and S_ISVTX, and S_IXGRP, which every
    // Unix numbers alike.
    let internal setUserId : int = 0o4000
    let internal setGroupId : int = 0o2000
    let internal sticky : int = 0o1000
    let internal groupExecute : int = 0o0010

    /// <summary>
    /// Render as an int.
    /// </summary>
    let toInt (bits : PermissionBits) : int =
        match bits with
        | PermissionBits bits -> bits

    // Whether the one triple `standing` selects lacks the bit `ownerBit` names
    // in the owner's triple: the owner's if the caller owns the object, else the
    // group's if it is in the object's group, else the other triple.
    //
    // Measured on Linux 6.18.5 (`permission-standing.c`, ext4 and tmpfs):
    // open for reading and for writing, a directory's search and read bits,
    // and creating an entry, over all 4096 modes for the owner, a member of
    // the group by its effective gid and by a supplementary group, anyone
    // else, and root, with no mismatch; and `access(2)` for every
    // combination of R_OK, W_OK and X_OK over the same modes and callers, on
    // a file and a directory (`access-rules.c`). Darwin 27.0 at uid 501: the
    // owner over all 4096 modes through `access(2)`, and spot rows for the
    // group and other triples (`ownership-probe.c`, and `access-rules.c`'s
    // scan of other users' inodes).
    let private selectedTripleLacks (standing : Standing) (ownerBit : int) (bits : PermissionBits) : bool =
        let bit =
            if standing.Owns then ownerBit
            elif standing.InGroup then ownerBit >>> 3
            else ownerBit >>> 6

        toInt bits &&& bit <> bit

    /// <summary>
    /// Whether a caller standing as <c>standing</c> towards an object carrying
    /// <c>bits</c> is refused <c>needed</c> on it.
    /// </summary>
    /// <remarks>
    /// Exactly one permission triple is consulted: the owner's if the caller owns
    /// the object, otherwise the group's if the caller is in the object's group,
    /// and otherwise the other triple. So an owner is refused what its own triple
    /// forbids even when the group or other triple would allow it.
    /// </remarks>
    /// <example>
    /// A <c>CallerPrivilege.Privileged</c> caller is refused nothing that
    /// <c>AccessRequest</c> can express, whatever the mode says.
    /// </example>
    let internal deniedTo (standing : Standing) (needed : AccessRequest) (bits : PermissionBits) : bool =
        match standing.Privilege, needed with
        // Root bypasses each of these three. It does not bypass executing a
        // non-directory, which is why `AccessRequest` cannot ask that; see
        // `executionDenied`.
        | CallerPrivilege.Privileged, AccessRequest.Read
        | CallerPrivilege.Privileged, AccessRequest.Write
        | CallerPrivilege.Privileged, AccessRequest.SearchDirectory -> false
        | CallerPrivilege.Unprivileged, _ ->

        let ownerBit =
            match needed with
            | AccessRequest.Read -> 0o400
            | AccessRequest.Write -> 0o200
            | AccessRequest.SearchDirectory -> 0o100

        selectedTripleLacks standing ownerBit bits

    /// <summary>
    /// Whether a caller standing as <c>standing</c> towards something that is not a
    /// directory, carrying <c>bits</c>, is refused executing it: <c>X_OK</c> on a regular
    /// file or a symbolic link.
    /// </summary>
    /// <remarks>
    /// An unprivileged caller is refused unless the triple its standing selects has the
    /// execute bit, exactly as <c>deniedTo</c> selects a triple. A privileged caller is
    /// <c>rule</c>'s to answer: see <c>PrivilegedExecution</c>.
    ///
    /// <c>X_OK</c> on a directory is search, which root is never refused; ask
    /// <c>deniedTo</c> with <c>AccessRequest.SearchDirectory</c> for that.
    /// </remarks>
    let internal executionDenied
        (rule : PrivilegedExecution)
        (standing : Standing)
        (bits : PermissionBits)
        : Result<bool, ExecutionRefusal>
        =
        // Measured by `access-rules.c` on Linux 6.18.5 (ext4): root, owning
        // the inode or not, over all 4096 modes, is granted X_OK on a regular
        // file exactly when `bits &&& 0o111` is nonzero, and on a symbolic
        // link asked about itself (0777) always. A real root with an effective
        // non-root uid is granted the same through `access(2)`.
        match standing.Privilege with
        | CallerPrivilege.Privileged ->
            match rule with
            | PrivilegedExecution.NeedsAnExecuteBit -> Ok (toInt bits &&& 0o111 = 0)
            | PrivilegedExecution.Unmeasured -> Error (ExecutionRefusal.UnmeasuredPrivilegedCaller (standing, bits))
        | CallerPrivilege.Unprivileged -> Ok (selectedTripleLacks standing 0o100 bits)

    /// <summary>
    /// Whether a caller standing as <c>standing</c> towards a symbolic link carrying
    /// <c>bits</c> is refused reading its target, under <c>rule</c>.
    /// </summary>
    /// <remarks>
    /// Under <c>LinkReadRule.ReadBitOfSelectedTriple</c> an unprivileged caller is
    /// judged exactly as <c>deniedTo</c> judges <c>AccessRequest.Read</c>, and a
    /// privileged one is refused an answer.
    /// </remarks>
    let internal linkReadDenied
        (rule : LinkReadRule)
        (standing : Standing)
        (bits : PermissionBits)
        : Result<bool, LinkReadRefusal>
        =
        // Measured by `readlink-mode.c`. Linux 6.18.5 (ext4, the modes set by
        // debugfs): every mode 0 to 07777, for a link uid 1000 owns, one in its
        // group and one in neither, read by uid 1000 and by root, all read.
        // Darwin 27.0 at uid 501: the owner over all 4096 modes, in the link's
        // group and out of it, read exactly when the owner's read bit is set;
        // root's links of 0700 (refused), 0644 and 0755 (read), and one of
        // 0755 in the caller's group (read).
        match rule with
        | LinkReadRule.ModeIgnored -> Ok false
        | LinkReadRule.ReadBitOfSelectedTriple ->
            match standing.Privilege with
            | CallerPrivilege.Privileged -> Error (LinkReadRefusal.UnmeasuredPrivilegedCaller (standing, bits))
            | CallerPrivilege.Unprivileged -> Ok (deniedTo standing AccessRequest.Read bits)

    /// What the sticky bit of a directory carrying <c>directoryBits</c> says about
    /// removing, renaming or replacing one of its entries, for a caller standing as
    /// `directory` towards the directory and as `entry` towards the inode the entry
    /// names.
    ///
    /// Which errno a kernel spends on `StickyRemoval.Forbidden`, where it falls among
    /// that syscall's other refusals, and what it does about
    /// `StickyRemoval.ForbiddenButPrivileged`, are each flavour's own: see
    /// `UnlinkRules.verdict`, `RmDirRules.verdict` and `RenameRules.verdict`.
    ///
    /// The two standings must be one caller's, so they must agree about its
    /// privilege; this throws if they do not.
    let internal stickyRemoval
        (directory : Standing)
        (entry : Standing)
        (directoryBits : PermissionBits)
        : StickyRemoval
        =
        if directory.Privilege <> entry.Privilege then
            failwith
                $"PermissionBits.stickyRemoval: the standing towards the directory (%O{directory}) and the standing towards the entry (%O{entry}) disagree about the caller's privilege, so they cannot be one caller's."


        if toInt directoryBits &&& sticky = 0 || directory.Owns || entry.Owns then
            StickyRemoval.Unrestricted
        else

        match directory.Privilege with
        | CallerPrivilege.Privileged -> StickyRemoval.ForbiddenButPrivileged
        | CallerPrivilege.Unprivileged -> StickyRemoval.Forbidden

    /// <summary>
    /// Parse a raw mode word's permission bits, or <c>None</c> if it does not fit in
    /// <c>0o7777</c>.
    /// </summary>
    /// <remarks>
    /// A caller passing a whole <c>st_mode</c> (including a type band) gets <c>None</c>
    /// rather than silently masking down the non-permission bits.
    /// </remarks>
    let parse (candidate : int) : PermissionBits option =
        if candidate < 0 || candidate > widest then
            None
        else
            Some (PermissionBits candidate)

    /// <summary>
    /// Parse a raw mode word's permission bits, throwing if the input doesn't fit in <c>0o7777</c>.
    /// </summary>
    /// <remarks>
    /// This is the throwing version of <c>parse</c>.
    /// </remarks>
    let parseOrFail (context : string) (candidate : int) : PermissionBits =
        match parse candidate with
        | Some bits -> bits
        | None ->
            failwith
                $"%s{context}: 0o%s{System.Convert.ToString (candidate, 8)} is not a permission word; it must lie in [0, 0o7777]. If this is a whole st_mode, mask off the S_IFMT band — the file type is derived from InodeContent, never stored."

    /// <summary>
    /// Re-check a value that crossed an API boundary.
    /// </summary>
    /// <remarks>
    /// You don't need to call this on the result of <c>PermissionBits.parse</c>; it's only for hand-constructed
    /// permissions or <c>Unchecked.defaultof</c>.
    /// </remarks>
    let assertValid (context : string) (bits : PermissionBits) : PermissionBits = parseOrFail context (toInt bits)

    /// <summary>
    /// Describe the permission bits you end up with when you create an inode with this
    /// <c>mode</c> argument, accounting for the platform-dependent default <c>modeMask</c>
    /// and the simulated process's <c>umask</c>.
    /// </summary>
    /// <param name="modeMask">
    /// The default mode mask for this syscall on this platform.
    ///
    /// This does depend on the syscall; for example, on Linux, under <c>umask 022</c>,
    /// the <c>mkdir(p, 0o7777)</c> syscall gives <c>0o1755</c>, while
    /// <c>open(p, O_CREAT, 0o7777)</c> instead gives <c>0o7755</c>.
    ///
    /// It also depends on the platform: Darwin drops all three upper bits in both those cases.
    ///
    /// See <c>CreatingOpenRules.ModeMask</c> or <c>MkDirRules.ModeMask</c>, for example,
    /// to find the specific mask you should provide here for the syscall you're executing.
    /// </param>
    ///
    /// <param name="umask">
    /// The process's file-mode creation mask, as its platform's <c>umask(2)</c> stored it, which is
    /// applied in full. The two platforms differ in what is stored rather than in what is applied:
    /// Linux stores only the low nine bits, and Darwin stores all twelve
    /// (see <c>SimulatedUnixPlatform.umaskStoredBits</c>). Darwin's stored special bits are
    /// invisible to <c>open(2)</c> and <c>mkdir(2)</c>, whose <c>modeMask</c> has already cleared
    /// them, but Darwin's <c>mkfifo(2)</c> keeps all twelve bits of its mode and they bite there.
    /// </param>
    ///
    /// <param name="mode">
    /// The integer which a process passes as a filemode argument to the syscall you're implementing.
    /// </param>
    ///
    /// <remarks>
    /// Any bits above the permission word are dropped rather than rejected.
    /// For example, <c>mode</c> of <c>0o10777</c> creates <c>0o0755</c> on both kernels.
    /// </remarks>
    let internal fromCreationMode (modeMask : PermissionBits) (umask : PermissionBits) (mode : int) : PermissionBits =
        mode &&& toInt modeMask &&& ~~~(toInt umask)
        |> parseOrFail "PermissionBits.fromCreationMode"


    /// <summary>
    /// What <c>chmod(2)</c> or <c>fchmod(2)</c>, asked for the raw mode word <c>mode</c> by a
    /// caller standing as <c>standing</c> towards an inode, does to that inode's mode.
    /// </summary>
    /// <remarks>
    /// Only the low twelve bits of <c>mode</c> are read; the rest are ignored rather than
    /// rejected, so <c>0o170644</c> and <c>-1</c> are <c>0o0644</c> and <c>0o7777</c>.
    ///
    /// An unprivileged caller that does not own the inode gets <c>ModeChange.Forbidden</c>,
    /// whatever it asks for and whether or not it is in the inode's group. One that owns it gets
    /// every bit it asks for, except that <c>S_ISGID</c> is silently dropped when it is not in the
    /// inode's group. That holds for directories as for regular files, and the sticky bit
    /// is kept on a regular file.
    ///
    /// What a privileged caller gets is <c>rule</c>'s to say; see <c>PrivilegedModeChange</c>.
    /// </remarks>
    let internal afterModeChange
        (rule : PrivilegedModeChange)
        (standing : Standing)
        (mode : int)
        : Result<ModeChange, ModeChangeRefusal>
        =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/chmod-rules.c`,
        // with no mismatch against this rule. Linux 6.18.5 (ext4 and tmpfs):
        // all 4096 modes on a file and on a directory, for the owner in the
        // inode's group by its effective gid and by a supplementary group,
        // outside it, a non-owner in each of those three relations, and root
        // owning or not, in the group or not. Every low word again under 21
        // high halves from `0o10000` to `0xFFFFF000`, none of which was read.
        // Darwin 27.0 at uid 501: the same sweeps for the owner in and out of
        // the group; a non-owner in the group and out of it, on a file and a
        // directory, asking for the mode the inode already had (EPERM).
        let requested = parseOrFail "PermissionBits.afterModeChange" (mode &&& widest)

        match standing.Privilege with
        | CallerPrivilege.Privileged ->
            match rule with
            | PrivilegedModeChange.SetsRequestedBits -> Ok (ModeChange.Permitted requested)
            | PrivilegedModeChange.Unmeasured ->
                Error (ModeChangeRefusal.UnmeasuredPrivilegedCaller (standing, requested))
        | CallerPrivilege.Unprivileged ->

        if not standing.Owns then
            Ok ModeChange.Forbidden
        elif standing.InGroup then
            Ok (ModeChange.Permitted requested)
        else
            Ok (ModeChange.Permitted (PermissionBits (toInt requested &&& ~~~setGroupId)))

    // When an unprivileged process changes a file's contents, Linux strips
    // `S_ISUID` whatever the execute bits say (`04644` becomes `00644`), and
    // `S_ISGID` if the file is group-executable or the writer is not in the
    // file's group. A member of the group keeps a `S_ISGID` without `S_IXGRP`,
    // which means mandatory locking rather than privilege: `02644` survives.
    // Owning the file makes no difference. The sticky bit is never touched.
    let private setIdBitsLinuxClears (standing : Standing) (raw : int) : int =
        let clearsGroup = raw &&& groupExecute <> 0 || not standing.InGroup
        setUserId ||| (if clearsGroup then setGroupId else 0)

    // Darwin's rules for a write and a truncation are measured only for a
    // writer who owns the file, and who is in its group when the file is
    // set-group-ID: an ordinary user cannot give a file it does not own a
    // set-ID bit, nor set `S_ISGID` on a file outside its groups. A file with
    // neither set-ID bit has nothing either rule could clear.
    let private darwinMeasured (standing : Standing) (bits : PermissionBits) : bool =
        let raw = toInt bits

        raw &&& (setUserId ||| setGroupId) = 0
        || standing.Owns
           && (standing.InGroup
               || raw &&& setGroupId = 0 && standing.Privilege = CallerPrivilege.Unprivileged)

    /// <summary>
    /// After a content-changing write to a regular file by a process standing as <c>standing</c>
    /// towards it, what permission bits now apply to the file?
    /// </summary>
    /// <remarks>
    /// This is a security measure imposed by the emulated platform: an unprivileged writer should not
    /// be able to edit a file and then invoke it as that file's owner (or group) with the new
    /// attacker-controlled contents.
    ///
    /// A write of no bytes is exempt from this check and should not consult
    /// this function.
    ///
    /// `S_ISUID` is blatted on both platforms whatever the execute bits say, and the
    /// sticky bit is never touched on either. The whole of the disagreement is
    /// `S_ISGID` on a file that is not group-executable.
    ///
    /// Answers <c>SetIdChangeRefusal.UnmeasuredDarwinWrite</c> for a write whose answer under
    /// <c>SetGroupIdOnWrite.StripAlways</c> has not been measured; see that case.
    /// </remarks>
    /// <param name="rule">
    /// Different platforms do different things to the set-group-ID on an unprivileged write.
    /// This parameter specifies what this platform does.
    /// </param>>
    /// <param name="standing">
    /// A privileged writer doesn't change any bits. Whether an unprivileged writer is in the file's
    /// group decides what Linux does to <c>S_ISGID</c>.
    /// </param>
    /// <param name="bits">
    /// The original permissions of the file before the writer wrote to it.
    /// </param>
    let internal afterContentChangingWrite
        (rule : SetGroupIdOnWrite)
        (standing : Standing)
        (bits : PermissionBits)
        : Result<PermissionBits, SetIdChangeRefusal>
        =
        // Measured on Linux 6.18.5 (`permission-standing.c`): every one of the
        // 4096 modes, written through a descriptor by the owner in and out of
        // the file's group, by a non-owner in the group by its effective gid
        // and by a supplementary group, by a non-owner outside it, and by root,
        // with no mismatch. On Darwin 27.0 at uid 501, the owner in the file's
        // group over all 4096 modes, and the owner outside it over the 2048
        // without `S_ISGID`. For example, non-root and in the file's group:
        //
        // | before | Linux | Darwin |
        // |---|---|---|
        // | `04755` | `00755` | `00755` |
        // | `04644` | `00644` | `00644` |
        // | `02755` | `00755` | `00755` |
        // | `02644` | `02644` | `00644` |
        // | `02600` | `02600` | `00600` |
        // | `02640` | `02640` | `00640` |
        // | `06755` | `00755` | `00755` |
        // | `06644` | `02644` | `00644` |
        // | `03755` | `01755` | `01755` |
        // | `01755` | `01755` | `01755` |
        // | `00644` | `00644` | `00644` |
        //
        // ...and as root every row is left exactly as it was, on both.
        match rule with
        | SetGroupIdOnWrite.StripAlways when not (darwinMeasured standing bits) ->
            Error (SetIdChangeRefusal.UnmeasuredDarwinWrite (standing, bits))
        | SetGroupIdOnWrite.StripAlways
        | SetGroupIdOnWrite.StripWhenGroupExecutableOrWriterOutsideGroup ->

        match standing.Privilege with
        | CallerPrivilege.Privileged -> Ok bits
        | CallerPrivilege.Unprivileged ->

        let raw = toInt bits

        let cleared =
            match rule with
            | SetGroupIdOnWrite.StripWhenGroupExecutableOrWriterOutsideGroup -> setIdBitsLinuxClears standing raw
            | SetGroupIdOnWrite.StripAlways -> setUserId ||| setGroupId

        Ok (parseOrFail "PermissionBits.afterContentChangingWrite" (raw &&& ~~~cleared))

    /// <summary>
    /// After a truncation of a regular file by a process standing as <c>standing</c> towards it,
    /// what permission bits now apply to the file?
    /// </summary>
    /// <remarks>
    /// <c>ftruncate(2)</c>, <c>O_TRUNC</c>, and an <c>ftruncate</c> to the length the file
    /// already has, are all observed to give the same answers, so this function will do for
    /// all three.
    ///
    /// If <c>rule</c> specifies that truncations strip, then <i>all</i> truncations strip,
    /// even ones which change no bytes.
    /// (This is by contrast to the situation with <c>afterContentChangingWrite</c>, which
    /// explicitly only applies to writes of at least one byte.)
    ///
    /// Answers <c>SetIdChangeRefusal.UnmeasuredDarwinTruncation</c> for a truncation whose answer
    /// under <c>SetIdBitsOnTruncation.Preserve</c> has not been measured; see that case.
    /// </remarks>
    let internal afterTruncation
        (rule : SetIdBitsOnTruncation)
        (standing : Standing)
        (bits : PermissionBits)
        : Result<PermissionBits, SetIdChangeRefusal>
        =
        // Measured as `afterContentChangingWrite` was, with `ftruncate(0)`,
        // `ftruncate` to the length the file already has, and (over the modes
        // the truncating process may open for writing) `open(O_TRUNC)`. For
        // example, non-root and in the file's group:
        //
        // | before | Linux | Darwin |
        // |---|---|---|
        // | `04755` | `00755` | `04755` |
        // | `04644` | `00644` | `04644` |
        // | `02755` | `00755` | `02755` |
        // | `02644` | `02644` | `02644` |
        // | `02600` | `02600` | `02600` |
        // | `02640` | `02640` | `02640` |
        // | `06755` | `00755` | `06755` |
        // | `06644` | `02644` | `06644` |
        // | `03755` | `01755` | `03755` |
        // | `01755` | `01755` | `01755` |
        //
        // ...and as root every row is left exactly as it was, on both.
        match rule, standing.Privilege with
        | SetIdBitsOnTruncation.Preserve, _ when not (darwinMeasured standing bits) ->
            Error (SetIdChangeRefusal.UnmeasuredDarwinTruncation (standing, bits))
        | SetIdBitsOnTruncation.Preserve, _
        | _, CallerPrivilege.Privileged -> Ok bits
        | SetIdBitsOnTruncation.Strip, CallerPrivilege.Unprivileged ->

        let raw = toInt bits
        Ok (parseOrFail "PermissionBits.afterTruncation" (raw &&& ~~~(setIdBitsLinuxClears standing raw)))
