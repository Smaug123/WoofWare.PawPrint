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
    /// An unprivileged write to the file does not change <c>S_IXGRP</c>.
    /// </summary>
    /// <remarks>
    /// On Linux, the <c>S_IXGRP</c> bit means "mandatory locking" rather than privilege,
    /// so a write can (and does) safely leave it alone.
    /// </remarks>
    /// <example>
    /// This is the case on Linux.
    ///
    /// For example, <c>02644</c> remains <c>02644</c> after an unprivileged write.
    /// </example>
    | StripWhenGroupExecutable
    /// <summary>
    /// Regardless of what the execute bits say, the group ID from <c>setgid</c> gets
    /// cleared after an unprivileged write.
    /// </summary>
    /// <remarks>
    /// On all platforms, <c>S_ISUID</c> already behaves this way.
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
    /// same bits that a normal write would clear.
    /// </remarks>
    /// <example>
    /// Linux behaves this way.
    /// </example>
    | Strip
    /// <summary>
    /// This Unix leaves <c>S_ISUID</c> and <c>S_ISGID</c> alone on file truncation,
    /// even if a write to the same file by the same process would strip them.
    /// </summary>
    /// <example>
    /// Darwin behaves this way.
    /// </example>
    | Preserve

/// <summary>
/// The permission, set-user-ID, set-group-ID and sticky bits of an inode's
/// mode.
/// </summary>
/// <remarks>
/// This is <c>st_mode & 0o7777</c>.
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

[<RequireQualifiedAccess>]
module PermissionBits =
    /// <summary>
    /// The widest <c>st_mode & 0o7777</c> can be:
    /// three rwx triples, plus setuid, setgid and the sticky bit.
    /// </summary>
    let private widest : int = 0o7777

    /// <summary>
    /// Render as an int.
    /// </summary>
    let toInt (bits : PermissionBits) : int =
        match bits with
        | PermissionBits bits -> bits

    /// <summary>
    /// Whether a caller with <c>privilege</c> is denied any of <c>needed</c> on an object
    /// carrying <c>bits</c>.
    /// </summary>
    ///
    /// <example>
    /// <c>CallerPrivilege.Privileged</c> (e.g. the root user) gets read and write whatever the mode says.
    /// </example>
    ///
    /// <remarks>
    /// Here are some currently-false statements, ported from some old docs, which we will fix
    /// immediately in the next commit when we refactor this code:
    ///
    /// Only *execute* still needs a bit set for root, and nothing that consults this asks for it.
    ///
    /// Only the owner triple can ever apply, for the reason
    /// `RemovalChecks.lacksWrite` gives: `stat` reports `Kernel.UserId` as every
    /// inode's `st_uid`, so the caller owns everything.
    /// </remarks>
    let deniedTo (privilege : CallerPrivilege) (needed : int) (bits : PermissionBits) : bool =
        match privilege with
        | CallerPrivilege.Privileged -> false
        | CallerPrivilege.Unprivileged -> toInt bits &&& needed <> needed

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

    /// The bits an inode created with this `mode` argument ends up with, under
    /// this platform's own mask and this process's `umask`.
    ///
    /// Two masks, in this order, and both measured. `modeMask` is the
    /// platform's, and it is per-*syscall* rather than per-platform: measured at
    /// `umask 022`, Linux's `mkdir(p, 0o7777)` gives 0o1755 while its
    /// `open(p, O_CREAT, 0o7777)` gives 0o7755, so `mkdir` keeps only the sticky
    /// bit where `open` keeps all twelve. Darwin drops all three upper bits from
    /// both. See `CreatingOpenRules.ModeMask` and `MkDirRules.ModeMask` for the
    /// values, which is where each syscall's rows live.
    ///
    /// Only the low nine bits of `umask` take part, and that is exact on both
    /// platforms for two *different* measured reasons. Linux's `umask(2)` stores
    /// just `mask &&& 0o777` -- `umask(0o4000)` reads back 0o0000 -- so a
    /// requested 0o4644 stays 0o4644 there; a mask applied at full width would
    /// strip the set-user-ID bit instead, making a setuid file impossible for a
    /// guest to create. Darwin *does* store all twelve bits, but creation cannot
    /// see the upper three because `modeMask` has already cleared them:
    /// measured, `umask 0o4000` with mode 0o4644 gives 0o0644 there whether or
    /// not the mask is truncated. So one expression is right for both.
    ///
    /// A bit above the permission word is dropped rather than rejected -- `mode`
    /// 0o10777 creates 0o0755 on both kernels, under `mkdir` as under `open`.
    let fromCreationMode (modeMask : PermissionBits) (umask : PermissionBits) (mode : int) : PermissionBits =
        let umaskBitsOnly = 0o777

        mode &&& toInt modeMask &&& ~~~(toInt umask &&& umaskBitsOnly)
        |> parseOrFail "PermissionBits.fromCreationMode"

    /// The set-ID bits a Linux kernel clears when an unprivileged process changes
    /// a file's contents.
    ///
    /// One rule, shared by `write(2)` and by truncation: measured non-root on
    /// Linux 6.18.5, `write`, `ftruncate`, `O_TRUNC` and a no-op `ftruncate`
    /// agree on all of `04755`, `04644`, `02755`, `02644`, `06755`, `02600`,
    /// `02640`, `06644`, `03755` and `01755`. Factored out so the bit arithmetic
    /// — which is where an off-by-one bit would hide — exists once.
    ///
    /// Linux only. Darwin strips nothing at all on truncation, and on a write
    /// strips `S_ISGID` unconditionally; see `SetGroupIdOnWrite`.
    ///
    /// `S_ISUID` goes whatever the execute bits say (`04644` becomes `00644`).
    /// `S_ISGID` goes only alongside `S_IXGRP`: without it the bit means
    /// mandatory locking rather than privilege, and `02644` survives. The sticky
    /// bit is never touched.
    let private setUserId : int = 0o4000
    let private setGroupId : int = 0o2000
    let private groupExecute : int = 0o0010

    let private setIdBitsLinuxClears (raw : int) : int =
        setUserId ||| (if raw &&& groupExecute <> 0 then setGroupId else 0)

    /// The bits a regular file is left with after a *content-changing* write by a
    /// process with `privilege`, on a kernel with this `rule`. A write that
    /// transfers nothing changes nothing, so a caller must not consult this for
    /// one.
    ///
    /// Total. Measured non-root on macOS 26.6 and Linux 6.18.5:
    ///
    /// | before | Linux | Darwin |
    /// |---|---|---|
    /// | `04755` | `00755` | `00755` |
    /// | `04644` | `00644` | `00644` |
    /// | `02755` | `00755` | `00755` |
    /// | `02644` | `02644` | `00644` |
    /// | `02600` | `02600` | `00600` |
    /// | `02640` | `02640` | `00640` |
    /// | `06755` | `00755` | `00755` |
    /// | `06644` | `02644` | `00644` |
    /// | `03755` | `01755` | `01755` |
    /// | `01755` | `01755` | `01755` |
    /// | `00644` | `00644` | `00644` |
    ///
    /// ...and as root every row is left exactly as it was, on both, which is what
    /// `CallerPrivilege.Privileged` selects.
    ///
    /// `S_ISUID` goes on both flavours whatever the execute bits say, and the
    /// sticky bit is never touched on either. The whole of the disagreement is
    /// `S_ISGID` on a file that is not group-executable, which is what `rule`
    /// names: on Linux the bit means mandatory locking rather than privilege and
    /// survives, and on Darwin it goes like any other set-ID bit. `06644` is the
    /// row worth not eliding — it carries both bits with no group-execute bit, so
    /// the two rules and "preserve everything" all answer it differently.
    let afterContentChangingWrite
        (rule : SetGroupIdOnWrite)
        (privilege : CallerPrivilege)
        (bits : PermissionBits)
        : PermissionBits
        =
        match privilege with
        | CallerPrivilege.Privileged -> bits
        | CallerPrivilege.Unprivileged ->

        let raw = toInt bits

        let cleared =
            match rule with
            | SetGroupIdOnWrite.StripWhenGroupExecutable -> setIdBitsLinuxClears raw
            | SetGroupIdOnWrite.StripAlways -> setUserId ||| setGroupId

        parseOrFail "PermissionBits.afterContentChangingWrite" (raw &&& ~~~cleared)

    /// The bits a regular file is left with after being truncated by a process
    /// with `privilege`, on a kernel with this `rule`.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5:
    ///
    /// | before | Linux | Darwin |
    /// |---|---|---|
    /// | `04755` | `00755` | `04755` |
    /// | `04644` | `00644` | `04644` |
    /// | `02755` | `00755` | `02755` |
    /// | `02644` | `02644` | `02644` |
    /// | `02600` | `02600` | `02600` |
    /// | `02640` | `02640` | `02640` |
    /// | `06755` | `00755` | `06755` |
    /// | `06644` | `02644` | `06644` |
    /// | `03755` | `01755` | `03755` |
    /// | `01755` | `01755` | `01755` |
    ///
    /// ...and as root every row is left exactly as it was, on both, which is what
    /// `CallerPrivilege.Privileged` selects.
    ///
    /// `ftruncate(2)`, `O_TRUNC`, and an `ftruncate` to the length the file
    /// already has all give the same answers, which is why one function serves
    /// all three. That last column is the one worth not eliding: **a truncation
    /// that changes no bytes still strips**, where a write of no bytes is not a
    /// write at all.
    let afterTruncation
        (rule : SetIdBitsOnTruncation)
        (privilege : CallerPrivilege)
        (bits : PermissionBits)
        : PermissionBits
        =
        match rule, privilege with
        | SetIdBitsOnTruncation.Preserve, _
        | _, CallerPrivilege.Privileged -> bits
        | SetIdBitsOnTruncation.Strip, CallerPrivilege.Unprivileged ->

        let raw = toInt bits
        parseOrFail "PermissionBits.afterTruncation" (raw &&& ~~~(setIdBitsLinuxClears raw))
