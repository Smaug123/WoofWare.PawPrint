namespace WoofWare.PosixKernel

open System.Buffers.Binary

/// Why a string is not usable as a `utsname.release`.
[<RequireQualifiedAccess>]
type SimulatedUnixReleaseError =
    /// Every Unix fills `utsname.release`, so the empty string names no system.
    | Empty
    /// Longer than any `utsname.release` can hold.
    | TooLong of length : int * limit : int
    /// The value is handed to the guest as a C string of single bytes, so a
    /// non-ASCII character has no faithful encoding and an embedded NUL would
    /// silently truncate what the guest sees.
    | NotPrintableAscii of index : int * character : char

/// Identity of the Unix-shaped platform the simulated process believes it is
/// running on. Consulted by the `SystemNative_*` entry points that report
/// host identity — today only `SystemNative_GetUnixRelease`, which surfaces
/// as `Environment.OSVersion` on a Unix CoreLib.
///
/// This is a value in kernel state rather than a host read, for the same
/// reason `ProcessorCount` is: real CoreCLR answers it from `uname(2)`, which
/// would make a replay depend on the machine that produced it — and worse,
/// guests branch on `Environment.OSVersion` (feature detection, quirk
/// workarounds), so letting the host leak in here would change guest
/// *control flow* between runs.
///
/// Modelled as a flavour plus a release string, rather than as a bag of loose
/// `utsname` fields, so that the facts we report stay mutually consistent as
/// more of `utsname` gets implemented: a future `SystemNative_GetUnixVersion`
/// or `SystemNative_GetOSArchitecture` is a new total *function* of the
/// flavour, not a new independently-settable string that could claim a Darwin
/// release alongside an x86_64 machine.
///
/// One representation per platform, which is what the flavour buys: every
/// platform-dependent fact below is a total function of it, with no failure
/// arms for an unclassifiable platform.
///
/// Construct with `SimulatedUnixPlatform.linuxX64`, `macOsArm64`, or `create`
/// for a specific release string.
[<CustomEquality ; NoComparison>]
type SimulatedUnixPlatform =
    private
        {
            Flavour : SimulatedUnixFlavour
            Release : string
        }

    override this.ToString () : string = $"%O{this.Flavour} %s{this.Release}"

    override this.Equals (other : obj) : bool =
        match other with
        | :? SimulatedUnixPlatform as other -> this.Flavour = other.Flavour && this.Release = other.Release
        | _ -> false

    override this.GetHashCode () : int =
        System.HashCode.Combine (this.Flavour, this.Release)

/// What the PAL puts in `DirectoryEntry.NameLength`, which is a fact about the
/// libc it was compiled against rather than about any directory.
///
/// `ConvertDirent` (`pal_io.c:497`) copies `d_namlen` under
/// `HAVE_DIRENT_NAME_LEN` and writes `-1` otherwise, the sentinel meaning "walk
/// to the NUL yourself". Established by compiling rather than by reading:
/// glibc's `struct dirent` has no `d_namlen` member at all (`gcc` rejects
/// `d.d_namlen`), while macOS's `sys/dirent.h` declares one.
///
/// Invisible to managed code — `DirectoryEntry.GetName` takes
/// `CreateReadOnlySpanFromNullTerminated` for the sentinel and a plain span
/// otherwise — so only a guest that hand-rolls the P/Invoke can tell.
[<RequireQualifiedAccess>]
type DirectoryEntryNameLength =
    /// The name's length in bytes, as macOS reports it.
    | Reported
    /// `-1`, as every libc without `d_namlen` gets.
    | WalkToTerminator

/// What `getcwd(3)` answers when the current directory has been *removed* — so
/// there is no path to report — and how small a buffer can still change that
/// answer.
///
/// Only reachable since `rmdir` could orphan a current directory. Measured on
/// both with the cwd removed out from under the process, sweeping the size from
/// 1 past the length of the path that used to be there: a zero-length buffer is
/// EINVAL everywhere (the shim's own guard, before `getcwd` is called at all),
/// and everything else splits on the *first byte* only.
[<RequireQualifiedAccess>]
type GetCwdOrphanAnswer =
    /// ENOENT whatever the size. Linux's `sys_getcwd` builds the path, fails
    /// because it is disconnected, and never reaches the length comparison —
    /// measured ENOENT at every size from 1 up.
    | AlwaysDetached
    /// ENOENT unless the buffer cannot hold even `"/"` and a terminator, which
    /// is ERANGE. Darwin's `getcwd(3)` builds the path from the root downwards,
    /// so it needs those two bytes before it can start; measured, size 1 is
    /// ERANGE and *every* larger size is ENOENT — including sizes far below the
    /// length of the path that used to be there. It is a minimum, not a
    /// comparison against a path that no longer exists.
    ///
    /// **This flavour's failing `getcwd` scribbles on the caller's buffer, and
    /// this library does not reproduce what it leaves.** `GetCwdAnswer.Failed`
    /// carries an errno and says nothing about the destination's contents; the
    /// errno itself is exact. Measured by sweeping the capacity with the
    /// destination prefilled `0xAA` and reporting every byte that changed:
    ///
    /// * orphaned, capacity 1: nothing written, ERANGE;
    /// * orphaned, 2 ≤ capacity < PATH_MAX: a NUL at the buffer's *last* byte;
    /// * orphaned, capacity ≥ PATH_MAX: that NUL, and the stale path at offset
    ///   0 as well;
    /// * intact but the path does not fit: a *suffix* of the path, filled
    ///   backwards from the last byte — 976 bytes at offsets 48..1023 for a
    ///   1418-byte path in a 1024-byte buffer — and ERANGE.
    ///
    /// That last shape is BSD `getcwd(3)` assembling the path backwards from
    /// the end of the buffer and moving it to the front once it fits, so the
    /// residue is a function of libc's internal progress rather than of
    /// anything a kernel decides. Reproducing it faithfully means reproducing
    /// that algorithm, including which of its paths a given capacity takes;
    /// reproducing it approximately means inventing bytes a guest can read. No
    /// caller in the BCL reads the destination after a NULL return, so this
    /// library reports the errno and leaves the buffer alone — recorded in
    /// `docs/divergences.md` rather than left to be discovered.
    ///
    /// Linux writes nothing on any failure path at any capacity, which is why
    /// only this case needs the note.
    | ShortestPathFirst

/// What an unwritable destination does to a `getcwd(3)` that has got as far as
/// storing into it — which is a question about *where the bytes are copied*,
/// and so splits by flavour rather than by kernel behaviour.
///
/// Measured with a destination that is mapped `PROT_READ` only, which
/// discriminates the two mechanisms where an unmapped address cannot: a kernel
/// copying with `copy_to_user` reports EFAULT, while a store executed in user
/// space takes a fatal signal. `readlink(2)` answers EFAULT on both platforms
/// in the same probe, so this is `getcwd`'s own property and not a general one.
[<RequireQualifiedAccess>]
type GetCwdDestinationFault =
    /// EFAULT, the destination untouched. Linux's `getcwd` is a syscall whose
    /// `copy_to_user` reports a bad destination as an ordinary error.
    | ReportedAsEfault
    /// A fatal signal — SIGSEGV for an unmapped destination, SIGBUS for a
    /// read-only one. Darwin's `getcwd(3)` assembles the path with stores
    /// executed in the caller's own context, so a destination it cannot write
    /// kills the process instead of producing an errno.
    ///
    /// A kernel cannot answer this, and neither can this library: see
    /// `GetCwdRefusal.FatalToTheProcess` for what it says instead.
    | FatalToTheProcess

/// What a `getsockname(2)` that faults copying the address out has already put
/// in the caller's length cell.
///
/// The two kernels order the two stores differently, so a call that fails
/// leaves the caller's `socklen_t` reading different things. Measured against a
/// wholly unmapped destination and against one writable for its first few bytes
/// only, with sentinel lengths of 7, 13, 100 and 4096 so that a value that came
/// back changed can only have been written: on Linux 6.18.5 every one of them
/// reads 16 afterwards, and on macOS 26.6 every one still reads what it went in
/// with. A descriptor that fails earlier -- EBADF, ENOTSOCK -- touches the cell
/// on neither, so this is the fault path's property rather than the failure
/// path's in general.
///
/// Whether a *client* can see this is a separate question, and for the .NET PAL
/// the answer is no: `SystemNative_GetSockName` copies the caller's length into
/// a local `socklen_t`, passes that, and writes it back only when the call
/// succeeded, so the kernel's store lands on the shim's stack. A client speaking
/// raw POSIX does see it.
[<RequireQualifiedAccess>]
type GetSockNameFaultLength =
    /// The cell still holds what the caller put there. Darwin copies the address
    /// out first and reports the length only once that has succeeded.
    | Untouched
    /// The cell holds the address's *untruncated* length -- what a successful
    /// call would have reported -- because the kernel stored that before
    /// attempting the copy that then faulted.
    | AlreadyReported

[<RequireQualifiedAccess>]
module SimulatedUnixPlatform =
    /// Loosest ceiling any Unix we model imposes on `utsname.release`:
    /// macOS's `_SYS_NAMELEN` is 256 (including the NUL), while Linux's
    /// `_UTSNAME_LENGTH` is only 65. Bounded by the looser of the two rather
    /// than per-flavour, because the limit is about what a *guest* can be
    /// handed rather than about which kernel wrote it, and an unbounded string
    /// could hand a guest a release no real `uname` could produce.
    [<Literal>]
    let private maxReleaseLength : int = 255

    let describe (error : SimulatedUnixReleaseError) : string =
        match error with
        | SimulatedUnixReleaseError.Empty ->
            "release string is empty, but every Unix `uname(2)` fills `utsname.release`"
        | SimulatedUnixReleaseError.TooLong (length, limit) ->
            $"release string is %d{length} characters, exceeding the %d{limit}-character limit any Unix `utsname.release` can hold"
        | SimulatedUnixReleaseError.NotPrintableAscii (index, character) ->
            $"release string contains non-printable-ASCII character U+%04X{int character} at index %d{index}; `utsname.release` is reported to the guest as single-byte characters, so only printable ASCII round-trips faithfully"

    /// A platform of the given flavour reporting `release` from `uname -r`.
    ///
    /// Validated here rather than when the release is read, which is what makes
    /// every accessor below total: a value of this type is a platform some Unix
    /// could actually be.
    let create
        (flavour : SimulatedUnixFlavour)
        (release : string)
        : Result<SimulatedUnixPlatform, SimulatedUnixReleaseError>
        =
        if System.String.IsNullOrEmpty release then
            Error SimulatedUnixReleaseError.Empty
        elif String.length release > maxReleaseLength then
            Error (SimulatedUnixReleaseError.TooLong (String.length release, maxReleaseLength))
        else

        match release |> Seq.tryFindIndex (fun c -> c < ' ' || c > '~') with
        | Some i -> Error (SimulatedUnixReleaseError.NotPrintableAscii (i, release.[i]))
        | None ->
            Ok
                {
                    Flavour = flavour
                    Release = release
                }

    let createOrFail (context : string) (flavour : SimulatedUnixFlavour) (release : string) : SimulatedUnixPlatform =
        match create flavour release with
        | Ok platform -> platform
        | Error error -> failwith $"%s{context}: %s{describe error}"

    /// 64-bit x86 Linux, at the exact kernel PawPrint's CI runs: the release
    /// this reports and the behaviour derived from it below therefore describe
    /// one real machine rather than a plausible composite. The default, and the
    /// flavour whose CoreLib actually routes `Environment.OSVersion` through
    /// `SystemNative_GetUnixRelease` at all (the macOS CoreLib goes via
    /// `Interop.libobjc.GetOperatingSystemVersion` instead).
    ///
    /// Naming a real kernel rather than a plausible one matters because facts
    /// derived from a platform are claims about a machine somebody could be
    /// running. Note the division of labour: identity that a guest reads back,
    /// like this release, belongs to the platform, because it is the same on
    /// every machine running this kernel image; a fact that varies between two
    /// machines running this very kernel, like the user-address limit, is a
    /// client's configuration instead.
    let linuxX64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.linuxX64" SimulatedUnixFlavour.Linux "6.17.0-1022-azure"

    /// 64-bit ARM macOS. The release is the *Darwin* kernel's, so `24.6.0`
    /// (macOS 15.6) rather than `15.6.0`.
    let macOsArm64 : SimulatedUnixPlatform =
        createOrFail "SimulatedUnixPlatform.macOsArm64" SimulatedUnixFlavour.Darwin "24.6.0"

    /// Which Unix this platform is.
    let flavour (platform : SimulatedUnixPlatform) : SimulatedUnixFlavour = platform.Flavour

    /// The `utsname.release` string this platform reports, i.e. exactly what
    /// `uname -r` would print. Part of PawPrint's replay contract: changing a
    /// preset's value changes the `Environment.OSVersion` every recorded trace
    /// on that platform observes.
    let unixRelease (platform : SimulatedUnixPlatform) : string = platform.Release

    /// Re-check the invariant of a value that may not have come from `create`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null release would otherwise
    /// be handed to a guest as its `uname -r`.
    let assertValid (context : string) (platform : SimulatedUnixPlatform) : SimulatedUnixPlatform =
        // A record is a reference type, so the forged value is `null` itself
        // rather than a record with a null field — and reading `Flavour` off it
        // would throw a `NullReferenceException` naming nothing useful.
        match box platform with
        | null ->
            failwith
                $"%s{context}: the platform is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create, or use the linuxX64 / macOsArm64 presets."
        | _ ->

        match create platform.Flavour platform.Release with
        | Ok _ -> platform
        | Error error ->
            failwith
                $"%s{context}: %s{describe error}. A SimulatedUnixPlatform that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create instead."

    /// Whose `<errno.h>` numbering this platform reports, for the errors where
    /// the two Unixes disagree.
    ///
    /// This is the choice `UnixError.toRawErrno` refuses to make on its own, and
    /// it is what lets an `ELOOP` reach a guest at all: raw 40 is `ELOOP` on
    /// Linux but `EMSGSIZE` on Darwin, so the number is meaningless until
    /// something says which Unix is being impersonated. The flavour says.
    let rawErrnoNumbering (platform : SimulatedUnixPlatform) : RawErrnoNumbering =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> RawErrnoNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> RawErrnoNumbering.Darwin

    /// Whose `<signal.h>` numbering this platform reports.
    ///
    /// Same shape as `rawErrnoNumbering`, and needed for the same reason: a
    /// signo says nothing until something names the Unix that assigned it.
    /// 17 is `SIGCHLD` on Linux and `SIGSTOP` on Darwin, so a guest that
    /// registers for `PosixSignal.SIGCHLD` must be handed 17 on the one and 20
    /// on the other, and one that hands 17 back must be told it cannot catch
    /// it on Darwin alone. `Signal.toRawSignoUnder` and its siblings take the
    /// answer.
    let signalNumbering (platform : SimulatedUnixPlatform) : SignalNumbering =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SignalNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> SignalNumbering.Darwin

    /// What this platform's `getcwd(3)` reports for a removed current directory.
    /// See `GetCwdOrphanAnswer`.
    let getCwdOrphanAnswer (platform : SimulatedUnixPlatform) : GetCwdOrphanAnswer =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetCwdOrphanAnswer.AlwaysDetached
        | SimulatedUnixFlavour.Darwin -> GetCwdOrphanAnswer.ShortestPathFirst

    /// What this platform's `getcwd(3)` does with a destination it cannot write.
    /// See `GetCwdDestinationFault`.
    let getCwdDestinationFault (platform : SimulatedUnixPlatform) : GetCwdDestinationFault =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetCwdDestinationFault.ReportedAsEfault
        | SimulatedUnixFlavour.Darwin -> GetCwdDestinationFault.FatalToTheProcess

    /// What this platform's `getsockname(2)` has already stored in the caller's
    /// length cell when the address copy faults. See `GetSockNameFaultLength`.
    let getSockNameFaultLength (platform : SimulatedUnixPlatform) : GetSockNameFaultLength =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GetSockNameFaultLength.AlreadyReported
        | SimulatedUnixFlavour.Darwin -> GetSockNameFaultLength.Untouched

    /// Whether the socket `accept(2)` hands back inherits `O_NONBLOCK` from the
    /// listening descriptor.
    ///
    /// The classic BSD/POSIX divergence, measured 2026-08-28 with
    /// `docs/plans/2026-08-23-posix-kernel-extraction/accept-inherits-nonblock.c`:
    /// on Linux 6.18.5 a non-blocking listener yields a *blocking* accepted
    /// socket, and on Darwin 25.6.0 a non-blocking one. Blocking listeners yield
    /// blocking sockets on both.
    ///
    /// This is the kernel's answer and not a runtime's. A client whose own
    /// sockets expect one answer everywhere has to normalise it -- CoreCLR's
    /// `SystemNative_Accept` clears the flag under `#if !defined(__linux__)`,
    /// with the comment "Our socket code expects new socket to be in blocking
    /// mode by default" -- and that normalisation belongs to the client rather
    /// than here.
    let acceptedSocketInheritsNonBlocking (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// What this platform's PAL puts in `DirectoryEntry.NameLength`. See
    /// `DirectoryEntryNameLength`.
    let directoryEntryNameLength (platform : SimulatedUnixPlatform) : DirectoryEntryNameLength =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> DirectoryEntryNameLength.WalkToTerminator
        | SimulatedUnixFlavour.Darwin -> DirectoryEntryNameLength.Reported

    /// Whether this platform's `stat` reports a creation time.
    ///
    /// A compile-time property of the native shim rather than of any file:
    /// `ConvertFileStatus` in `pal_io.c` sets `BirthTime` and the
    /// `HAS_BIRTHTIME` flag under `#if HAVE_STAT_BIRTHTIME` — true on macOS,
    /// false on Linux, where it hard-zeroes both with the comment "Linux path:
    /// until we use statx()". So the birth time is a real fact about the inode
    /// on both, and this governs only whether the guest is told it.
    let reportsBirthTime (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// The permission bits this platform reports for a symbolic link, which no
    /// syscall can set and which the two Unixes disagree about.
    ///
    /// Measured rather than read: with `umask 022` macOS reports 0o755 for a
    /// fresh symlink, with `umask 077` it reports 0o700 and with `umask 000`
    /// 0o777 — it applies the creating process's umask, exactly as it does to a
    /// regular file. Linux reports 0o777 whatever the umask, which is why
    /// `InodePermissions` derives this rather than storing it: under a Linux
    /// simulation a stored value could only ever describe a filesystem no
    /// kernel produced.
    ///
    /// The Darwin answer here is the `umask 022` one, and stays a constant even
    /// though a process umask is modelled: a symbolic link can only enter this
    /// filesystem through a *seed*, and a seed describes a tree some other
    /// process built, so this run's configured umask is not the one that applied
    /// to it. The day a `symlink(2)` lets a guest create one, that link *is*
    /// created by this process and this must become a function of the configured
    /// umask — that is the trigger, not the existence of the field.
    let symlinkPermissions (platform : SimulatedUnixPlatform) : PermissionBits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" 0o777
        | SimulatedUnixFlavour.Darwin ->
            PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkPermissions" (0o777 &&& ~~~0o022)

    /// Whether this platform clears a truncated file's set-user-ID and
    /// set-group-ID bits.
    ///
    /// The only thing about truncation the two Unixes disagree about — every
    /// other row measured (the errno order, which descriptors refuse, the
    /// zero-fill, the timestamps, and `O_TRUNC`'s extra write-permission
    /// requirement) is unanimous, which is why this is a lone value rather than a
    /// `CreatingOpenRules`-shaped record.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, for `ftruncate(2)`,
    /// `O_TRUNC` and a no-op `ftruncate` alike; `PermissionBits.afterTruncation`
    /// carries the table. Linux applies the same rule it applies to a write.
    /// **Darwin strips nothing at all**, and that is isolated rather than
    /// inferred: in one process, on one file, a one-byte `write` takes `04755` to
    /// `00755` there while `ftruncate` leaves it `04755`.
    let setIdBitsOnTruncation (platform : SimulatedUnixPlatform) : SetIdBitsOnTruncation =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetIdBitsOnTruncation.Strip
        | SimulatedUnixFlavour.Darwin -> SetIdBitsOnTruncation.Preserve

    /// Whether this platform's content-changing `write(2)` clears `S_ISGID` on a
    /// file that is not group-executable.
    ///
    /// The only thing about a write's effect on the mode that the two Unixes
    /// disagree about: `S_ISUID` goes on both whatever the execute bits say, and
    /// the sticky bit is left alone by both. So this is a lone value rather than
    /// a `CreatingOpenRules`-shaped record, for the reason
    /// `setIdBitsOnTruncation` above gives.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, one byte written over
    /// the front of a four-byte file; `PermissionBits.afterContentChangingWrite`
    /// carries the table. Linux applies to a write the same rule it applies to a
    /// truncation, and **Darwin does not** — there a write strips `02644` to
    /// `00644` while an `ftruncate` on the same file leaves the whole mode alone,
    /// which is why the two rules are separate values rather than one.
    ///
    /// The file must be handed to a group the caller belongs to before `chmod`,
    /// or the kernel drops `S_ISGID` silently and the measurement reads as
    /// agreement.
    let setGroupIdOnWrite (platform : SimulatedUnixPlatform) : SetGroupIdOnWrite =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetGroupIdOnWrite.StripWhenGroupExecutable
        | SimulatedUnixFlavour.Darwin -> SetGroupIdOnWrite.StripAlways

    /// How this platform's `open(2)` behaves when asked to create; see
    /// `CreatingOpenRules` for what each field means and how it was measured.
    let creatingOpenRules (platform : SimulatedUnixPlatform) : CreatingOpenRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.RefuseIsDirectory
                RefusesExistingDirectory = true
                RootNavigation = None
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o7777
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RefusesExistingDirectory = false
                RootNavigation = Some UnixError.EEXIST
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o0777
            }

    /// Everything this platform's `mkdir(2)` does differently. See `MkDirRules`
    /// for the measurements; note in particular that `ModeMask` is not
    /// `creatingOpenRules`' one on Linux.
    let mkDirRules (platform : SimulatedUnixPlatform) : MkDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o1777
                InheritsSetGroupIdFromParent = true
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.mkDirRules" 0o0777
                InheritsSetGroupIdFromParent = false
            }

    /// Everything this platform's `unlink(2)` does differently. See
    /// `UnlinkRules`, whose one field this picks; the rest of the divergence is
    /// in `UnlinkRules.verdict`, which takes the flavour directly.
    let unlinkRules (platform : SimulatedUnixPlatform) : UnlinkRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
            }

    /// Everything this platform's `rmdir(2)` does differently. See `RmDirRules`,
    /// whose two fields this picks; the ordering half of the divergence is in
    /// `RmDirRules.verdict`, which takes the flavour directly.
    let rmDirRules (platform : SimulatedUnixPlatform) : RmDirRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                RemovedDirectoryEffect = UnbindTargetEffect.LostALink
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RemovedDirectoryEffect = UnbindTargetEffect.Untouched
            }

    /// Everything this platform's `rename(2)` does differently. See
    /// `RenameRules`, whose two fields this picks; the ordering of the refusals
    /// — which is most of the divergence — is in `RenameRules.verdict`, which
    /// takes the flavour directly.
    let renameRules (platform : SimulatedUnixPlatform) : RenameRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                WalkOrder = RenameWalkOrder.ParentsThenFinals
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                WalkOrder = RenameWalkOrder.SourceThenDestination
            }

    /// Whether this platform's kernel screens a read or write buffer before it
    /// performs the operation.
    ///
    /// Linux's `vfs_read`/`vfs_write` (fs/read_write.c) reject an out-of-range
    /// buffer with EFAULT between the descriptor's access-mode check and the
    /// file operation, so the fault beats EISDIR and fires for a zero-length
    /// request. macOS screens nothing up front, so a call that transfers no
    /// bytes never looks at the buffer: measured, `read(f, (void*)-1, 5)` on a
    /// descriptor at end-of-file is EFAULT on Linux and 0 on macOS.
    ///
    /// *Where* it screens is the machine's `UserAddressLimit`, not a property
    /// of the flavour: both architectures compare the range end against
    /// `TASK_SIZE_MAX` (`valid_user_address` against `USER_PTR_MAX` in
    /// arch/x86/include/asm/uaccess_64.h, and the
    /// `(u65)addr + (u65)size <= (u65)TASK_SIZE_MAX` that
    /// arch/arm64/include/asm/uaccess.h documents), and that value varies with
    /// paging depth and virtual-address width — measured, two GitHub runners in
    /// one CI run disagreed. A caller combines the two: this predicate decides
    /// *whether* there is an up-front check, and its own configured limit says
    /// what that check compares against.
    let screensUserBufferUpFront (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The bounds this platform's kernel puts on path resolution.
    ///
    /// The numbers are measured facts about real kernels, which is why they are
    /// derived from the flavour rather than configured: a host that could set
    /// them could describe a Unix that does not exist, and a guest would then
    /// see a `MAXSYMLINKS` no real system has. `TestVirtualFileSystemAgainstHost`
    /// pins the value for whichever flavour it is running on against that
    /// kernel's *measured* behaviour, so macOS locally and Linux in CI each
    /// check one column.
    /// `PATH_MAX` counts the NUL, so the usable lengths are one less: measured,
    /// an argument of 1023 bytes resolves on macOS and 1024 does not, and 4095
    /// and 4096 respectively on Linux.
    ///
    /// `NAME_MAX` is 255 on both — but *of different things*, which is why it
    /// carries its unit. See `NameLengthLimit`: `中`×255 is 765 bytes and 255
    /// UTF-16 units, and APFS resolves it where ext4 refuses it.
    let pathLimits (platform : SimulatedUnixPlatform) : PathLimits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PathLimits.create 40 4096 (NameLengthLimit.Utf8Bytes 255) SpliceLengthRecheck.NoRecheck
        | SimulatedUnixFlavour.Darwin ->
            PathLimits.create 32 1024 (NameLengthLimit.Utf16CodeUnits 255) SpliceLengthRecheck.Recheck

    /// `sizeof(struct sockaddr_storage)`: the size of the largest socket address
    /// any Unix we model can hand back, and so the buffer size CoreLib sizes
    /// every socket-address buffer by. Reported to the guest by
    /// `SystemNative_GetMaximumAddressSize`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`. Unlike that one it takes no flavour: both families
    /// *define* the constant in their headers rather than computing it
    /// (`_SS_MAXSIZE` on Darwin, `_SS_SIZE` in glibc's generic `bits/sockaddr.h`)
    /// and derive the padding members from it, so the value is invariant of
    /// pointer width as well as agreed between the two — both descend from
    /// RFC 2553's sample definition. Measured 128 on macOS arm64 and on Linux
    /// alike, and re-pinned against a real platform on every test run by
    /// `sourcesPure/SystemNativeGetMaximumAddressSize.cs`. Make it a function of
    /// the flavour on the day one of them disagrees.
    ///
    /// Contrast `sockaddr_un`, which genuinely does differ (106 on Darwin, 110 on
    /// Linux). That is `SocketAddressSizes.UnixDomain` below, reported through a
    /// different entry point again; this binding is where the shared 128 is
    /// defined, and `socketAddressSizes` reads it rather than repeating it.
    let maximumSocketAddressSize : int = 128

    /// The sizes `SystemNative_GetSocketAddressSizes` reports. See
    /// `SocketAddressSizes` for where each number was measured.
    let socketAddressSizes (platform : SimulatedUnixPlatform) : SocketAddressSizes =
        {
            InterNetwork = 16
            InterNetworkV6 = 28
            UnixDomain =
                match flavour platform with
                | SimulatedUnixFlavour.Linux -> 110
                | SimulatedUnixFlavour.Darwin -> 106
            Storage = maximumSocketAddressSize
        }

    /// The order `bind(2)` reports its faults in, which is **not** the same on
    /// the two flavours.
    ///
    /// Measured pairwise, by presenting each pair of faults together and seeing
    /// which errno came back. Linux checks the declared length before it reads
    /// the family, and defers "this socket is already bound" until after it has
    /// validated the address; Darwin reads the family first and rejects an
    /// already-bound socket before it looks at the address at all. So
    /// a rebind to a non-local address is `EADDRNOTAVAIL` on Linux and `EINVAL`
    /// on Darwin, and a short `sockaddr_in6` on an IPv4 socket is `EINVAL` on
    /// Linux and `EAFNOSUPPORT` on Darwin.
    ///
    /// Expressed as an order over faults rather than as nested branches so that
    /// the divergence is one list rather than two code paths, and so a test can
    /// assert the order directly.
    let bindFaultOrder (platform : SimulatedUnixPlatform) : BindFault list =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            [
                BindFault.Length
                BindFault.Family
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AlreadyBound
                BindFault.AddressInUse
            ]
        | SimulatedUnixFlavour.Darwin ->
            [
                BindFault.Family
                BindFault.Length
                BindFault.AlreadyBound
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AddressInUse
            ]

    /// The first fault in this platform's order that `faults` contains.
    let firstBindFault (platform : SimulatedUnixPlatform) (faults : Set<BindFault>) : BindFault option =
        bindFaultOrder platform |> List.tryFind (fun fault -> Set.contains fault faults)

    /// How long `bind(2)` insists a `struct sockaddr_in` argument is.
    ///
    /// Measured, and not the same shape on the two: Linux accepts any length from
    /// the family's own `sizeof` up to `sizeof(struct sockaddr_storage)` — 16
    /// through 128 inclusive for IPv4, with 129 the least rejected — while Darwin
    /// insists on exactly 16 and answers `EINVAL` for every value from 17 to 32.
    ///
    /// Invisible through the managed API, which always passes
    /// `SocketAddress.Size`; a hand-rolled `[DllImport]` sees it immediately.
    /// The greatest `socketAddressLen` Darwin's `bind(2)` will consider at all.
    /// Above it the answer is `ENAMETOOLONG` rather than `EINVAL`; measured, 255
    /// is `EINVAL` and 256 is `ENAMETOOLONG`. Linux has no such threshold.
    let maximumDarwinSocketAddressLength : int = 255

    let bindAddressLength (platform : SimulatedUnixPlatform) (exactSize : int) (declared : int) : BindLengthVerdict =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            if declared > maximumSocketAddressSize then
                BindLengthVerdict.RejectedBeforeCopy UnixError.EINVAL
            elif declared >= exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid
        | SimulatedUnixFlavour.Darwin ->
            if declared > maximumDarwinSocketAddressLength then
                BindLengthVerdict.RejectedBeforeCopy UnixError.ENAMETOOLONG
            elif declared = exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid

    /// May a socket bind to this address, given the addresses this machine holds?
    ///
    /// The wildcard always binds. Beyond that the flavours read the same list
    /// differently, which is measured rather than inferred: `127.9.9.9` binds on
    /// Linux and is `EADDRNOTAVAIL` on Darwin, because Linux treats every address
    /// inside a local prefix as assigned while Darwin assigns loopback exactly
    /// one address.
    ///
    /// Is this the all-ones broadcast address, or a multicast one
    /// (`224.0.0.0/4`)?
    ///
    /// **PawPrint refuses to bind either**, rather than answering. Measured, the
    /// rule is not one rule: Linux takes both on a stream socket, Darwin answers
    /// `EAFNOSUPPORT` there, and on Darwin the answer depends on the socket's
    /// *kind* besides — a datagram socket binds a multicast group where a stream
    /// socket does not. Modelling that is modelling multicast, which is group
    /// membership and an interface to receive on, and PawPrint has neither; a
    /// bind that succeeded here would become a lie the moment `recvfrom` landed.
    ///
    /// So this classifier exists to *refuse* precisely, at the point in
    /// `bindFaultOrder` where the address is judged — a fault the platform ranks
    /// earlier still wins, which is what keeps the refusal from swallowing
    /// answers PawPrint does know.
    let isBroadcastOrMulticast (address : uint32) : bool =
        address = System.UInt32.MaxValue || (address >>> 28) = 0xEu

    /// Broadcast and multicast are a further Linux-only allowance
    /// (`255.255.255.255` and `224.0.0.1` bind there and are `EAFNOSUPPORT` on
    /// Darwin). Neither is modelled: PawPrint has no interface to broadcast on,
    /// and the entry point refuses such an address rather than answering, so a
    /// guest that needs one gets a diagnosis instead of a wrong errno.
    let isBindableAddress
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        if address = InternetEndpoint.WildcardAddress then
            true
        elif List.contains address localAddresses then
            // An address this machine holds binds on either flavour.
            true
        else

        match flavour platform with
        // Linux additionally takes anything it has a *local route* to, which is
        // why `127.9.9.9` binds there. An interface's subnet is not such a route
        // — holding `192.168.1.10/24` does not make `192.168.1.11` bindable — so
        // this reads the route table rather than widening the assigned addresses.
        | SimulatedUnixFlavour.Linux -> localRoutes |> List.exists (Ipv4Prefix.contains address)
        | SimulatedUnixFlavour.Darwin -> false

    /// Whether `bind(2)` has something to say about the address itself, as
    /// opposed to about the length, the family, or another socket. Callers rank
    /// this against the other faults in `bindFaultOrder`, at
    /// `BindFault.AddressNotLocal`.
    ///
    /// That is `EADDRNOTAVAIL` in every case PawPrint answers. A broadcast or
    /// multicast address faults here too, and its caller refuses it outright
    /// rather than reporting an errno — which is why this is not simply
    /// `not isBindableAddress`. Such an address is not necessarily *unbindable*:
    /// Linux binds `224.0.0.1` on a stream socket quite happily. It is one
    /// PawPrint declines to answer for, and a host that listed it in
    /// `LocalAddresses`, or covered it with a `LocalRoutes` prefix, would
    /// otherwise silence the refusal and record a multicast binding that nothing
    /// downstream can honour.
    let bindAddressFaults
        (platform : SimulatedUnixPlatform)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        isBroadcastOrMulticast address
        || not (isBindableAddress platform localAddresses localRoutes address)

    /// Does a bind of `candidate` collide with the socket already bound at
    /// `existing`?
    ///
    /// Both flavours refuse two sockets the same port on overlapping addresses,
    /// and both relax that when `SO_REUSEADDR` is set — in opposite directions,
    /// which is the whole of the divergence here and is measured in both:
    ///
    /// * **Linux** relaxes only while nothing is listening. Two sockets that both
    ///   set the flag may share an address, exactly or through the wildcard,
    ///   until one of them calls `listen(2)`; after that the second bind is
    ///   `EADDRINUSE`.
    /// * **Darwin** relaxes only for addresses that differ. Two sockets that both
    ///   set the flag may hold the wildcard and a specific address on one port,
    ///   listening or not; the exact duplicate is `EADDRINUSE` either way.
    ///
    /// With the flag absent on either side — every UDP bind through the shim, and
    /// every `ProtocolType.Unspecified` one — the two agree and refuse.
    ///
    /// The same relation answers `listen(2)`, which is measured rather than
    /// assumed: on Linux two reuse-carrying sockets may share an endpoint until
    /// one listens, and the *second* `listen` is then EADDRINUSE — exactly what
    /// this says when the other socket is already listening. Darwin never refuses
    /// a listen, and never lets the pair coexist in the first place.
    let bindConflict
        (platform : SimulatedUnixPlatform)
        (existing : SocketBinding)
        (existingReuse : bool)
        (existingPhase : SocketPhase)
        (candidate : SocketBinding)
        (candidateReuse : bool)
        : bool
        =
        if existing.Endpoint.Port <> candidate.Endpoint.Port then
            false
        elif not (InternetEndpoint.addressesOverlap existing.Endpoint candidate.Endpoint) then
            false
        else

        let existingIsListening = SocketPhase.isListening existingPhase

        // An established socket's pcb is keyed by its full peer tuple, and a
        // replacement listener can bind over it: measured on both kernels
        // (accept a connection, close the listener, bind a reuse-carrying
        // replacement at the exact endpoint — OK; without the candidate's
        // reuse flag — EADDRINUSE).
        let existingIsEstablished =
            match existingPhase with
            | SocketPhase.Established _
            | SocketPhase.EstablishedPendingReport _ -> true
            | SocketPhase.Idle
            | SocketPhase.Listening _
            | SocketPhase.RefusedPendingDelivery
            | SocketPhase.Dead
            | SocketPhase.DatagramPeer _ -> false

        match flavour platform with
        // Linux relaxes only while nothing listens, and only when *both* sockets
        // carry the flag. That rule already answers the measured established
        // rows correctly: an established child carries its listener's flag, so
        // a reuse-carrying rebind over it passes and a flagless one conflicts.
        | SimulatedUnixFlavour.Linux -> not (existingReuse && candidateReuse) || existingIsListening
        // Darwin relaxes only for addresses that differ, and keys on the
        // *candidate's* flag alone — measured: a wildcard listener that
        // `listen(2)` bound implicitly carries no flag at all, and a later
        // reuse-carrying bind to a specific address on its port still succeeds.
        // The exact-duplicate refusal exempts established sockets (measured
        // above).
        | SimulatedUnixFlavour.Darwin ->
            (existing.Endpoint.Address = candidate.Endpoint.Address
             && not existingIsEstablished)
            || not candidateReuse

    /// Whether `listen(2)` on a socket that is *already bound* asks the port
    /// admission question again, so that a binding admitted earlier can still be
    /// refused a listen.
    ///
    /// The flavours differ, and not merely in strictness. Linux's
    /// `inet_csk_listen_start` calls `get_port` a second time, which is why two
    /// sockets carrying SO_REUSEADDR may share an endpoint right up until one of
    /// them listens; Darwin's `tcp_usr_listen` binds only when the socket has no
    /// port yet, so an already-bound listen consults nothing. Both measured.
    ///
    /// This is not a strictness knob that could be left on for safety. Darwin's
    /// bind rule is asymmetric in SO_REUSEADDR -- it keys on the *candidate's*
    /// flag alone -- so re-asking it at listen time asks with the roles swapped,
    /// and a pair admitted at bind time answers the other way. Re-checking there
    /// would invent an EADDRINUSE, not merely tighten one.
    let listenRescreensBinding (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// Where this platform keeps a socket address's family, and how wide it is.
    /// See `SockaddrFamilyField`, which is also where the reason every other
    /// field's offset is flavour-free is written down.
    let sockaddrFamilyField (platform : SimulatedUnixPlatform) : SockaddrFamilyField =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SockaddrFamilyField.TwoBytesAtOffsetZero
        | SimulatedUnixFlavour.Darwin -> SockaddrFamilyField.OneByteAtOffsetOne

    /// Whether this platform's sockets report IPv4 packet information on a
    /// dual-mode socket — an IPv6 socket receiving IPv4-mapped traffic. Reported
    /// to the guest by `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`.
    ///
    /// A compile-time property of the native shim rather than of any socket, like
    /// `reportsBirthTime`: upstream the whole function body is
    /// `#if HAVE_SUPPORT_FOR_DUAL_MODE_IPV4_PACKET_INFO return 1 #else return 0`,
    /// and `configure.cmake` sets that define to 1 for every Linux target and
    /// leaves it 0 elsewhere. There is no probe of the running kernel involved, so
    /// this is not a fact about the machine but about which shim was built.
    ///
    /// (Linux includes Android here: the `NOT CLR_CMAKE_TARGET_ANDROID` test
    /// nested inside that `if` scopes only a `CMAKE_REQUIRED_LIBRARIES` setting,
    /// not the define.)
    ///
    /// Follows the flavour rather than conservatively reporting `false`
    /// everywhere, because both of CoreLib's readers of it are guest-visible
    /// control flow (see the handler arm for which): answering `false` while
    /// impersonating Linux makes a guest see a `PlatformNotSupportedException`
    /// real Linux does not raise, and does so silently, with no abort and no
    /// diagnostic.
    ///
    /// Answering `true` carries an obligation for whoever implements the socket
    /// emulation this leads on to: a Linux-flavour `recvmsg` on a dual-mode
    /// socket must actually produce the IPv4 `pktinfo` control message, because
    /// CoreLib latches this once per process and will thereafter ask for the
    /// packet information and expect to be given it. Reporting support and then
    /// handing back a default `IPPacketInformation` would be the data-level
    /// version of the lie this function exists to avoid.
    let supportsDualModeIPv4PacketInfo (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The stride of the event buffer `SystemNative_CreateSocketEventBuffer`
    /// allocates and `SystemNative_WaitForSocketEvents` fills, in bytes.
    ///
    /// A compile-time property of the native shim, like `reportsBirthTime`:
    /// `pal_networking.c` defines `SocketEventBufferElementSize` once per backend,
    /// as `max(sizeof(struct epoll_event), sizeof(SocketEvent))` under epoll and
    /// `sizeof(struct kevent)` under kqueue.
    ///
    /// Note what the epoll `max` does, because it is the reason this is a total
    /// function of the flavour where `LinuxEpollLimits.EventSize` is not.
    /// `sizeof(struct epoll_event)` is architecture-dependent — 12 on x86-64 under
    /// `EPOLL_PACKED`, 16 everywhere else — and the `max` against the 16-byte
    /// `SocketEvent` erases exactly that difference, since `max(12, 16)` and
    /// `max(16, 16)` are both 16. So the buffer stride follows the flavour alone,
    /// while the `epoll_wait` constants that skip the `max` do not.
    ///
    /// `sizeof(struct kevent)` is 32 on every 64-bit Darwin:
    /// `{ uintptr_t ident; int16_t filter; uint16_t flags; uint32_t fflags;
    /// intptr_t data; void* udata; }`, measured rather than recalled.
    let socketEventBufferElementSize (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 16
        | SimulatedUnixFlavour.Darwin -> 32

    /// What `fcntl(F_SETFL)` answers on a socket event port — `None` for
    /// success — the `O_NONBLOCK` bit having changed *either way*.
    ///
    /// Measured, not derived: on Linux 6.18.5 the call succeeds and the flag
    /// round-trips; on Darwin (through the real shim's
    /// `SystemNative_FcntlSetIsNonBlocking`, macOS 26) it returns -1 with
    /// ENOTTY and a subsequent `F_GETFL` nevertheless reports the toggled bit,
    /// in both directions. So the caller must store the flag first and then
    /// report this answer.
    ///
    /// The stored bit changes no modelled wait: both `epoll_wait` and `kevent`
    /// take their blocking behaviour from their own timeout argument rather
    /// than from the descriptor's status flags, so
    /// `SystemNative_WaitForSocketEvents` rightly never consults it.
    let eventPortSetStatusFlagsError (platform : SimulatedUnixPlatform) : UnixError option =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> None
        | SimulatedUnixFlavour.Darwin -> Some UnixError.ENOTTY

    /// `AF_INET`, in the platform's own numbering. 2 on both, and on essentially
    /// every Unix — it is one of the handful of `AF_*` values that predate the
    /// BSD/Linux split and never moved.
    ///
    /// Exposed alongside `internetV6AddressFamily` because the `sockaddr`
    /// accessors switch on the raw `sa_family` in the blob rather than on a
    /// converted value: `SystemNative_GetPort` is a `switch (sockAddr->sa_family)`
    /// over exactly these two, and `SystemNative_GetIPv4Address` is an equality
    /// against the first.
    let internetAddressFamily : int = 2

    /// Ports a process may bind only as root.
    ///
    /// Measured as 1024 on both: binding 1023 is `EACCES` for an unprivileged
    /// caller and 1024 succeeds. A constant rather than a function of the
    /// platform because the two agree, and not configuration though Linux does
    /// expose it as `ip_unprivileged_port_start` -- nothing needs to vary it
    /// yet, and a knob with no consumer is a knob no test covers.
    let privilegedPortCeiling : uint16 = 1024us

    /// `AF_INET6`, in the platform's own numbering, which unlike `AF_INET` the two
    /// families disagree about: 10 on Linux against 30 on Darwin. Measured.
    let internetV6AddressFamily (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 10
        | SimulatedUnixFlavour.Darwin -> 30

    /// `struct sockaddr_in` for `endpoint`, as this platform's kernel copies one
    /// out: the family, the port and the address, and on the flavours that have
    /// the field, the `sa_len` byte in front of them.
    ///
    /// The copy-*out* direction specifically. Measured: a Darwin `getsockname`
    /// on a bound socket reports `10 02 ...`, the leading `0x10` being the
    /// 16-byte length, so the kernel fills `sa_len` in even though nothing in a
    /// runtime's shim writes it. `SockaddrFamilyField.OneByteAtOffsetOne`
    /// describes the same byte travelling the other way, where it is a caller's
    /// own store; the two do not disagree.
    ///
    /// Answers the struct's full length for the platform, so a caller bounded by
    /// a shorter declared length truncates what it writes rather than asking for
    /// a shorter blob.
    let encodeInternetSockaddr (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : byte[] =
        let realLength = (socketAddressSizes platform).InterNetwork
        let blob = Array.zeroCreate<byte> realLength

        BinaryPrimitives.WriteUInt16BigEndian (
            System.Span<byte> (blob, InternetSockaddr.port.Offset, InternetSockaddr.port.Width),
            endpoint.Port
        )

        BinaryPrimitives.WriteUInt32BigEndian (
            System.Span<byte> (blob, InternetSockaddr.address.Offset, InternetSockaddr.address.Width),
            endpoint.Address
        )

        let field = sockaddrFamilyField platform
        let familyOffset = SockaddrFamilyField.offset field

        match SockaddrFamilyField.width field with
        | 1 ->
            blob.[familyOffset] <- byte internetAddressFamily
            // Written only on the flavour that has the field -- on Linux those
            // two bytes are the family itself.
            blob.[0] <- byte realLength
        | _ ->
            BinaryPrimitives.WriteUInt16LittleEndian (
                System.Span<byte> (blob, familyOffset, 2),
                uint16 internetAddressFamily
            )

        blob

    /// The socket shapes both flavours create for an unprivileged process.
    let private portableCreatableSockets : (SocketDomain * SocketKind * SocketProtocol) list =
        [
            SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.InterNetwork, SocketKind.Stream, SocketProtocol.Tcp
            SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Unspecified
            SocketDomain.InterNetwork, SocketKind.Datagram, SocketProtocol.Udp
            SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.InterNetworkV6, SocketKind.Stream, SocketProtocol.Tcp
            SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Unspecified
            SocketDomain.InterNetworkV6, SocketKind.Datagram, SocketProtocol.Udp
            SocketDomain.Unix, SocketKind.Stream, SocketProtocol.Unspecified
            SocketDomain.Unix, SocketKind.Datagram, SocketProtocol.Unspecified
        ]

    /// The two Linux adds, and they are the kernel's own divergence rather than
    /// any shim's: Darwin answers `EPROTONOSUPPORT` for both from `socket(2)`,
    /// having passed every screen a caller's runtime could apply.
    let private linuxOnlyCreatableSockets : (SocketDomain * SocketKind * SocketProtocol) list =
        [
            SocketDomain.Unix, SocketKind.Raw, SocketProtocol.Unspecified
            SocketDomain.Unix, SocketKind.SeqPacket, SocketProtocol.Unspecified
        ]

    let private linuxCreatableSockets : Set<SocketDomain * SocketKind * SocketProtocol> =
        Set.ofList (portableCreatableSockets @ linuxOnlyCreatableSockets)

    let private darwinCreatableSockets : Set<SocketDomain * SocketKind * SocketProtocol> =
        Set.ofList portableCreatableSockets

    /// Every socket shape this emulated kernel creates, under `platform`. A
    /// `socket(2)` for anything else is refused rather than answered.
    ///
    /// This is the kernel's declared protocol table, and it is deliberately
    /// smaller than what the platform would really create. The rows outside it
    /// are absent for three different reasons — some are privilege-dependent
    /// (every raw and packet socket: measured, 70 Linux rows change answer
    /// between euid 1000 and euid 0), some sysctl-dependent (Linux's ping
    /// sockets, gated by `net.ipv4.ping_group_range`), and some deterministic
    /// but simply not modelled. A shape outside this set is a socket PawPrint
    /// has not decided how to be, and refusing leaves that decision open where
    /// a guessed errno would not.
    ///
    /// Exposed as data rather than as a predicate because the set is the fact:
    /// a caller deciding whether to create one wants to ask, and a reader
    /// wanting to know what this kernel is wants to enumerate.
    let creatableSockets (platform : SimulatedUnixPlatform) : Set<SocketDomain * SocketKind * SocketProtocol> =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> linuxCreatableSockets
        | SimulatedUnixFlavour.Darwin -> darwinCreatableSockets
