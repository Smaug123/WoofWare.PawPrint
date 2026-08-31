namespace WoofWare.PosixKernel

/// Whether the calling process is exempt from the file-permission rules — which
/// is one question, asked by several of them, because the emulated kernel has a
/// single identity and being exempt means being uid 0.
///
/// The rules that ask: whether a write or a truncation strips a file's
/// set-user-ID and set-group-ID bits, and whether a permission bit is consulted
/// at all before reading, writing, binding a name, or searching a directory on
/// the way through a path.
///
/// A DU rather than a `bool`, so that a caller cannot silently pass the wrong
/// one of two adjacent flags, and so that the answer arrives at a signature
/// saying what it means.
[<RequireQualifiedAccess>]
type CallerPrivilege =
    /// uid 0. Measured on Linux as root: a write to an `04755` file leaves it
    /// `04755`, and a directory whose mode is 0o000 can still be searched.
    | Privileged
    /// Any other identity.
    | Unprivileged

/// Whether this Unix clears `S_ISGID` when an unprivileged process changes a
/// file's contents, on a file that is not group-executable.
///
/// The one thing about a content-changing write the two platforms disagree
/// about, so it is derived from `SimulatedUnixFlavour` rather than crashed on;
/// see `SimulatedUnixPlatform.setGroupIdOnWrite`, which is where the measured
/// table lives.
///
/// Only `S_ISGID` needs a rule. `S_ISUID` is cleared by both flavours in every
/// measured row, and the sticky bit is left alone by both, so a DU spanning
/// those too would carry a case no platform selects.
[<RequireQualifiedAccess>]
type SetGroupIdOnWrite =
    /// Linux: without `S_IXGRP` the bit means mandatory locking rather than
    /// privilege, so a write leaves it alone — `02644` survives.
    | StripWhenGroupExecutable
    /// Darwin: the bit goes whatever the execute bits say, exactly as `S_ISUID`
    /// does — `02644` becomes `00644`.
    | StripAlways

/// Whether this Unix clears a truncated file's set-user-ID and set-group-ID bits.
///
/// The one thing about truncation the two platforms disagree about, so it is
/// derived from `SimulatedUnixFlavour` rather than crashed on; see
/// `SimulatedUnixPlatform.setIdBitsOnTruncation`, which is where the measured
/// table lives.
[<RequireQualifiedAccess>]
type SetIdBitsOnTruncation =
    /// Linux: truncating is a content change like any other, and clears the same
    /// bits a write clears.
    | Strip
    /// Darwin: truncating leaves the whole mode alone — even where a *write* to
    /// the same file by the same process would strip it.
    | Preserve

/// The permission, set-user-ID, set-group-ID and sticky bits of an inode's
/// mode: `st_mode & 0o7777`, which is exactly `chmod(2)`'s domain.
///
/// Deliberately *not* the `S_IFMT` file-type band, which is derived from
/// `InodeContent` by `InodeContent.fileTypeBits` instead. `chmod(2)` cannot
/// set the type band either, so keeping it out of the stored value
/// makes "the recorded type disagrees with the content" unrepresentable.
[<Struct>]
type PermissionBits =
    private
    | PermissionBits of bits : int

    override this.ToString () : string =
        match this with
        | PermissionBits bits -> "0o" + System.Convert.ToString(bits, 8).PadLeft (4, '0')

[<RequireQualifiedAccess>]
module PermissionBits =
    /// The widest `st_mode & 0o7777` can be: three rwx triples, plus setuid,
    /// setgid and the sticky bit.
    let private widest : int = 0o7777

    let toInt (bits : PermissionBits) : int =
        match bits with
        | PermissionBits bits -> bits

    /// Whether a caller with `privilege` is denied any of `needed` on an object
    /// carrying `bits`.
    ///
    /// Root gets read and write whatever the mode says — measured on Linux as
    /// uid 0, where a mode-0000 file opens for writing. Only *execute* still
    /// needs a bit set for root, and nothing that consults this asks for it.
    ///
    /// Only the owner triple can ever apply, for the reason
    /// `RemovalChecks.lacksWrite` gives: `stat` reports `Kernel.UserId` as every
    /// inode's `st_uid`, so the caller owns everything.
    ///
    /// Shared by `SystemNative_Open` and `OpenDirRules`, which ask the same
    /// question of the same object — `opendir(3)` is an
    /// `open(O_RDONLY | O_DIRECTORY)`, and the read bit it demands is the one
    /// `open` was already demanding. Two copies of the rule could drift apart in
    /// a way no differential test would catch, since a real runtime would agree
    /// with itself either way.
    let deniedTo (privilege : CallerPrivilege) (needed : int) (bits : PermissionBits) : bool =
        match privilege with
        | CallerPrivilege.Privileged -> false
        | CallerPrivilege.Unprivileged -> toInt bits &&& needed <> needed

    /// Parse a raw mode word's permission bits, or `None` if it does not fit in
    /// `0o7777`.
    ///
    /// A caller passing a whole `st_mode` (type band included) is refused
    /// rather than silently masked down.
    let parse (candidate : int) : PermissionBits option =
        if candidate < 0 || candidate > widest then
            None
        else
            Some (PermissionBits candidate)

    let parseOrFail (context : string) (candidate : int) : PermissionBits =
        match parse candidate with
        | Some bits -> bits
        | None ->
            failwith
                $"%s{context}: 0o%s{System.Convert.ToString (candidate, 8)} is not a permission word; it must lie in [0, 0o7777]. If this is a whole st_mode, mask off the S_IFMT band — the file type is derived from InodeContent, never stored."

    /// Re-check a value that crossed an API boundary, so a forged
    /// `Unchecked.defaultof` or a hand-built out-of-range word is refused where
    /// it enters rather than where it is next read.
    ///
    /// The zero it cannot catch is a real mask (`umask 000`), so this guards the
    /// range rather than the default.
    let assertValid (context : string) (bits : PermissionBits) : PermissionBits = parseOrFail context (toInt bits)

    /// What a `umask 022` process gets from `open(2)` with the 0o666 that
    /// CoreLib's `FileStream` passes: `0o666 &&& ~~~0o022`.
    ///
    /// A constant, and deliberately *not* derived from `EmulatedKernel.Umask`
    /// even though that now exists: this is the default mode of a **seed** entry,
    /// and a seed describes a tree that some other process built, so this run's
    /// configured mask has no bearing on it. Deriving it would make raising the
    /// mask silently change what an unannotated seed entry means.
    let defaultForRegularFile : PermissionBits = PermissionBits (0o666 &&& ~~~0o022)

    /// What a `umask 022` process gets from `mkdir(2)`'s 0o777:
    /// `0o777 &&& ~~~0o022`. See `defaultForRegularFile`.
    let defaultForDirectory : PermissionBits = PermissionBits (0o777 &&& ~~~0o022)

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
