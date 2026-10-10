namespace WoofWare.PosixKernel

open System.Buffers.Binary
open System.Collections.Immutable

/// Why a string is not usable as a `utsname.release`.
[<RequireQualifiedAccess>]
type SimulatedUnixReleaseError =
    /// Every Unix fills `utsname.release`, so the empty string names no system.
    | Empty
    /// Longer than any `utsname.release` can hold.
    | TooLong of length : int * limit : int
    /// The value is handed to a process as a C string of single bytes, so a
    /// non-ASCII character has no faithful encoding and an embedded NUL would
    /// silently truncate what the process sees.
    | NotPrintableAscii of index : int * character : char

/// A Linux kernel's version: the `VERSION`, `PATCHLEVEL` and `SUBLEVEL` of the
/// source tree it was built from, which a real `uname -r` begins with.
///
/// The facts this library derives from Linux's source that changed between
/// versions follow this, never the platform's release string: the release is
/// what a process reads back, which a client may set to anything a `uname`
/// could print, while this says which source's behaviour the kernel has.
///
/// Ordered by `Major`, then `Minor`, then `Patch`, which is the order of the
/// fields, so the structural comparison F# derives is the version order.
type LinuxKernelVersion =
    {
        /// The source tree's `VERSION`: 6 in 6.17.0.
        Major : uint32
        /// The source tree's `PATCHLEVEL`: 17 in 6.17.0.
        Minor : uint32
        /// The source tree's `SUBLEVEL`, the stable release: 0 in 6.17.0.
        Patch : uint32
    }

    override this.ToString () : string =
        $"%d{this.Major}.%d{this.Minor}.%d{this.Patch}"

/// Which kernel a `SimulatedUnixPlatform` runs, with whatever the facts this
/// library derives from that kernel's source vary over.
[<RequireQualifiedAccess>]
type SimulatedUnixKernel =
    /// Linux, built from this version's source.
    | Linux of LinuxKernelVersion
    /// Darwin. No fact this library derives from Darwin's source varies between
    /// its versions yet, so none is recorded.
    | Darwin

[<RequireQualifiedAccess>]
module SimulatedUnixKernel =
    /// Which Unix this kernel is.
    let flavour (kernel : SimulatedUnixKernel) : SimulatedUnixFlavour =
        match kernel with
        | SimulatedUnixKernel.Linux _ -> SimulatedUnixFlavour.Linux
        | SimulatedUnixKernel.Darwin -> SimulatedUnixFlavour.Darwin

/// Why a combination of kernel, architecture, page size and release is not a
/// `SimulatedUnixPlatform`.
[<RequireQualifiedAccess>]
type SimulatedUnixPlatformError =
    /// The release string is not one a `uname` could report.
    | Release of SimulatedUnixReleaseError
    /// No kernel of this flavour, built for this architecture with pages of this
    /// size, has been measured, so the facts this library derives from the
    /// three are unknown for it.
    | UnmeasuredKernel of
        flavour : SimulatedUnixFlavour *
        architecture : SimulatedUnixArchitecture *
        pageSize : SimulatedPageSize

/// Identity of the Unix-shaped platform the simulated process believes it is
/// running on: what `uname(2)` reports, and the flavour every other
/// platform-dependent answer follows.
///
/// This is a value in kernel state rather than a host read, for the same
/// reason `ProcessorCount` is: reading the host's `uname(2)` would make a
/// replay depend on the machine that produced it — and worse, programs branch
/// on the platform they find (feature detection, quirk workarounds), so
/// letting the host leak in here would change their *control flow* between
/// runs.
///
/// Modelled as a kernel (a flavour, and for Linux the version of the source it
/// was built from), an architecture, a page size and a release string, each of
/// which a kernel image is built with, rather than as a bag of loose
/// `utsname` fields, so that the facts we report stay mutually consistent as
/// more of `utsname` gets modelled: its machine field would be a total
/// *function* of the flavour and the architecture, not an independently-settable
/// string that could claim an x86_64 machine alongside arm64's struct layouts.
///
/// Every platform-dependent fact below is a total function of these, with no
/// failure arms for an unclassifiable platform, because `create` admits only
/// combinations whose facts have been measured.
///
/// The release string is identity alone: no fact follows from it. A fact that
/// changed between Linux versions follows the kernel's `LinuxKernelVersion`.
///
/// Construct with `SimulatedUnixPlatform.linuxX64`, `linuxArm64`, `macOsArm64`,
/// or `create` for a specific kernel and release string.
[<CustomEquality ; NoComparison>]
type SimulatedUnixPlatform =
    private
        {
            Kernel : SimulatedUnixKernel
            Architecture : SimulatedUnixArchitecture
            PageSize : SimulatedPageSize
            Release : string
        }

    override this.ToString () : string =
        let kernel =
            match this.Kernel with
            | SimulatedUnixKernel.Linux version -> $"Linux %O{version}"
            | SimulatedUnixKernel.Darwin -> "Darwin"

        $"%s{kernel} %O{this.Architecture} %O{this.PageSize} %s{this.Release}"

    override this.Equals (other : obj) : bool =
        match other with
        | :? SimulatedUnixPlatform as other ->
            this.Kernel = other.Kernel
            && this.Architecture = other.Architecture
            && this.PageSize = other.PageSize
            && this.Release = other.Release
        | _ -> false

    override this.GetHashCode () : int =
        System.HashCode.Combine (this.Kernel, this.Architecture, this.PageSize, this.Release)

/// What `getcwd(3)` answers when the current directory has been *removed* — so
/// there is no path to report — and how small a buffer can still change that
/// answer.
///
/// Only reachable since `rmdir` could orphan a current directory. Measured on
/// both with the cwd removed out from under the process, sweeping the size from
/// 1 past the length of the path that used to be there: a zero-length buffer is
/// EINVAL everywhere (libc's own `getcwd(3)` guard, before the kernel is asked),
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
    /// reproducing it approximately means inventing bytes a process can read.
    /// So this library reports the errno and leaves the buffer alone, a
    /// divergence only a caller that reads the destination after a NULL return
    /// could see — recorded in `docs/divergences.md` rather than left to be
    /// discovered.
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
/// The kernels order the two stores differently, so a call that fails leaves
/// the caller's `socklen_t` reading different things. Darwin, and Linux before
/// 6.18, store the length only once the copy has succeeded; Linux from 6.18
/// stores it first. Measured through a null destination and an unmapped page,
/// with declared lengths of 1, 7, 13, 16, 100 and 4096, so that a value that
/// came back changed can only have been written
/// (`docs/probes/sockname-fault-length`): every one reads 16 afterwards on
/// Linux 6.18.5 (x86-64 and aarch64), and every one still reads what it went in
/// with on Linux 6.12.111 and 6.17.13 (x86-64) and Darwin 27.0.0. A descriptor
/// that fails earlier -- EBADF, ENOTSOCK -- touched the cell on neither Linux
/// 6.18.5 nor macOS 26.6, so this is the fault path's property rather than the
/// failure path's in general.
[<RequireQualifiedAccess>]
type GetSockNameFaultLength =
    /// The cell still holds what the caller put there: the kernel copies the
    /// address out first and reports the length only once that has succeeded.
    | Untouched
    /// The cell holds the address's *untruncated* length -- what a successful
    /// call would have reported -- because the kernel stored that before
    /// attempting the copy that then faulted.
    | AlreadyReported

/// What a kernel does with the buffer size a `readlink(2)` caller passed,
/// before it copies anything. `SimulatedUnixPlatform.readlinkCapacity`.
[<RequireQualifiedAccess>]
type internal ReadLinkCapacityVerdict =
    /// The call fails with this errno before the path is resolved: a missing
    /// path is answered the same way.
    | Refuse of UnixError
    /// The path is resolved and must name a symbolic link, and the call then
    /// reports zero bytes without consulting the buffer.
    | ReportNothing
    /// The size is one the copy can honour.
    | Admit

/// What a platform's `read(2)`, `write(2)`, `pread(2)` and `pwrite(2)` do with
/// a count larger than one call moves. The count itself is a `size_t`, so any
/// value up to `UInt64.MaxValue` can be asked for.
[<RequireQualifiedAccess>]
type TransferCountLimit =
    /// A larger count is not an error: the call moves at most `maxTransfer`
    /// bytes, and returns how many it moved.
    ///
    /// The count is shortened only after the buffer's range and the file
    /// position have been checked, and both of those checks see the whole count
    /// the caller asked for.
    | Shortened of maxTransfer : int
    /// A larger count is EINVAL, and that is the first thing the call answers:
    /// before the descriptor is looked up, the buffer is looked at, or anything
    /// else about the call is decided.
    | Refused of maxTransfer : int

[<RequireQualifiedAccess>]
module TransferCountLimit =
    /// The most bytes one call moves, whether a larger count is shortened to it
    /// or refused.
    let maxTransfer (limit : TransferCountLimit) : int =
        match limit with
        | TransferCountLimit.Shortened maxTransfer
        | TransferCountLimit.Refused maxTransfer -> maxTransfer

[<RequireQualifiedAccess>]
module SimulatedUnixPlatform =
    /// Loosest ceiling any Unix we model imposes on `utsname.release`:
    /// macOS's `_SYS_NAMELEN` is 256 (including the NUL), while Linux's
    /// `_UTSNAME_LENGTH` is only 65. Bounded by the looser of the two rather
    /// than per-flavour, because the limit is about what a *process* can be
    /// handed rather than about which kernel wrote it, and an unbounded string
    /// could hand a process a release no real `uname` could produce.
    [<Literal>]
    let private maxReleaseLength : int = 255

    let describe (error : SimulatedUnixReleaseError) : string =
        match error with
        | SimulatedUnixReleaseError.Empty ->
            "release string is empty, but every Unix `uname(2)` fills `utsname.release`"
        | SimulatedUnixReleaseError.TooLong (length, limit) ->
            $"release string is %d{length} characters, exceeding the %d{limit}-character limit any Unix `utsname.release` can hold"
        | SimulatedUnixReleaseError.NotPrintableAscii (index, character) ->
            $"release string contains non-printable-ASCII character U+%04X{int character} at index %d{index}; `utsname.release` is reported to a process as single-byte characters, so only printable ASCII round-trips faithfully"

    /// Why a combination is not a platform, for a message.
    let describeError (error : SimulatedUnixPlatformError) : string =
        match error with
        | SimulatedUnixPlatformError.Release error -> describe error
        | SimulatedUnixPlatformError.UnmeasuredKernel (flavour, architecture, pageSize) ->
            $"no %O{flavour} kernel for %O{architecture} with %d{SimulatedPageSize.bytes pageSize}-byte pages has been measured, so the facts that follow from the architecture and the page size are unknown for it"

    // The combinations whose architecture- and page-size-dependent facts have
    // been measured, by
    // docs/plans/2026-08-23-posix-kernel-extraction/architecture-facts-linux.c
    // and architecture-facts-darwin.c on 2026-09-26:
    //
    //   Linux x86-64, 4 KiB: Debian's 6.12 kernel under QEMU (x86-64 has no
    //     other base page size).
    //   Linux arm64, 4 KiB: 6.18.5 under Apple's `container`. arm64 kernels are
    //     also built for 16 and 64 KiB pages, which nothing here has measured.
    //   Darwin arm64, 16 KiB: Darwin 27.0.0 on Apple silicon.
    //
    // Darwin x86-64 is not admitted: macOS 27 runs on no Intel machine, and the
    // measuring machine has no Rosetta to run an x86-64 process under.
    let private isMeasured
        (flavour : SimulatedUnixFlavour)
        (architecture : SimulatedUnixArchitecture)
        (pageSize : SimulatedPageSize)
        : bool
        =
        match flavour, architecture, pageSize with
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.X64, SimulatedPageSize.FourKiB
        | SimulatedUnixFlavour.Linux, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.FourKiB
        | SimulatedUnixFlavour.Darwin, SimulatedUnixArchitecture.Arm64, SimulatedPageSize.SixteenKiB -> true
        | _ -> false

    /// A platform running `kernel`, built for `architecture` with pages of
    /// `pageSize`, reporting `release` from `uname -r`.
    ///
    /// `release` need not agree with a Linux kernel's version: it is what a
    /// process reads, and nothing else follows from it.
    ///
    /// Validated here rather than when a fact is read, which is what makes
    /// every accessor below total: a value of this type is a platform some Unix
    /// could actually be, and one whose facts are known. A combination nobody has
    /// measured is refused rather than answered from a neighbouring one.
    let create
        (kernel : SimulatedUnixKernel)
        (architecture : SimulatedUnixArchitecture)
        (pageSize : SimulatedPageSize)
        (release : string)
        : Result<SimulatedUnixPlatform, SimulatedUnixPlatformError>
        =
        if System.String.IsNullOrEmpty release then
            Error (SimulatedUnixPlatformError.Release SimulatedUnixReleaseError.Empty)
        elif String.length release > maxReleaseLength then
            Error (
                SimulatedUnixPlatformError.Release (
                    SimulatedUnixReleaseError.TooLong (String.length release, maxReleaseLength)
                )
            )
        else

        match release |> Seq.tryFindIndex (fun c -> c < ' ' || c > '~') with
        | Some i ->
            Error (SimulatedUnixPlatformError.Release (SimulatedUnixReleaseError.NotPrintableAscii (i, release.[i])))
        | None ->

        let flavour = SimulatedUnixKernel.flavour kernel

        if not (isMeasured flavour architecture pageSize) then
            Error (SimulatedUnixPlatformError.UnmeasuredKernel (flavour, architecture, pageSize))
        else

        Ok
            {
                Kernel = kernel
                Architecture = architecture
                PageSize = pageSize
                Release = release
            }

    let createOrFail
        (context : string)
        (kernel : SimulatedUnixKernel)
        (architecture : SimulatedUnixArchitecture)
        (pageSize : SimulatedPageSize)
        (release : string)
        : SimulatedUnixPlatform
        =
        match create kernel architecture pageSize release with
        | Ok platform -> platform
        | Error error -> failwith $"%s{context}: %s{describeError error}"

    /// 64-bit x86 Linux with 4 KiB pages, at a kernel release a real machine was running (a
    /// GitHub Actions Ubuntu runner's): the release this reports, and the
    /// kernel version 6.17.0 that release names and the behaviour below follows,
    /// therefore describe one real machine rather than a plausible composite.
    /// `UnixSystem.defaultUnixPlatform`.
    ///
    /// Naming a real kernel rather than a plausible one matters because facts
    /// derived from a platform are claims about a machine somebody could be
    /// running. Note the division of labour: identity that a process reads back,
    /// like this release, belongs to the platform, because it is the same on
    /// every machine running this kernel image; a fact that varies between two
    /// machines running this very kernel, like the user-address limit, is a
    /// client's configuration instead.
    let linuxX64 : SimulatedUnixPlatform =
        createOrFail
            "SimulatedUnixPlatform.linuxX64"
            (SimulatedUnixKernel.Linux
                {
                    Major = 6u
                    Minor = 17u
                    Patch = 0u
                })
            SimulatedUnixArchitecture.X64
            SimulatedPageSize.FourKiB
            "6.17.0-1022-azure"

    /// 64-bit ARM Linux with 4 KiB pages, at the release of the kernel most of
    /// this library's Linux behaviour was measured against (Apple's
    /// `container`), for the same reason `linuxX64` names a real one.
    let linuxArm64 : SimulatedUnixPlatform =
        createOrFail
            "SimulatedUnixPlatform.linuxArm64"
            (SimulatedUnixKernel.Linux
                {
                    Major = 6u
                    Minor = 18u
                    Patch = 5u
                })
            SimulatedUnixArchitecture.Arm64
            SimulatedPageSize.FourKiB
            "6.18.5"

    /// 64-bit ARM macOS 27.0, whose pages are 16 KiB. The release is the
    /// *Darwin* kernel's, so `27.0.0` rather than `27.0`; as for `linuxX64`, it
    /// names the kernel the Darwin behaviour below was measured against.
    let macOsArm64 : SimulatedUnixPlatform =
        createOrFail
            "SimulatedUnixPlatform.macOsArm64"
            SimulatedUnixKernel.Darwin
            SimulatedUnixArchitecture.Arm64
            SimulatedPageSize.SixteenKiB
            "27.0.0"

    /// Which Unix this platform is.
    let flavour (platform : SimulatedUnixPlatform) : SimulatedUnixFlavour =
        SimulatedUnixKernel.flavour platform.Kernel

    /// Which kernel this platform runs, and for Linux the version of the source
    /// it was built from.
    let kernel (platform : SimulatedUnixPlatform) : SimulatedUnixKernel = platform.Kernel

    /// The instruction set this platform's processes run as.
    let architecture (platform : SimulatedUnixPlatform) : SimulatedUnixArchitecture = platform.Architecture

    /// The size of this platform's pages.
    let pageSize (platform : SimulatedUnixPlatform) : SimulatedPageSize = platform.PageSize

    /// The `utsname.release` string this platform reports, i.e. exactly what
    /// `uname -r` would print. Part of every replay's input: changing a
    /// preset's value changes what every recorded trace on that platform
    /// observed from `uname(2)`.
    let unixRelease (platform : SimulatedUnixPlatform) : string = platform.Release

    /// Re-check the invariant of a value that may not have come from `create`.
    /// See `FileName.assertValid`: the only value this can reject is
    /// `Unchecked.defaultof` / C# `default`, whose null release would otherwise
    /// be handed to a process as its `uname -r`.
    let assertValid (context : string) (platform : SimulatedUnixPlatform) : SimulatedUnixPlatform =
        // A record is a reference type, so the forged value is `null` itself
        // rather than a record with a null field — and reading `Flavour` off it
        // would throw a `NullReferenceException` naming nothing useful.
        match box platform with
        | null ->
            failwith
                $"%s{context}: the platform is null, which it can only be if it came from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create, or use the linuxX64 / linuxArm64 / macOsArm64 presets."
        | _ ->

        match create platform.Kernel platform.Architecture platform.PageSize platform.Release with
        | Ok _ -> platform
        | Error error ->
            failwith
                $"%s{context}: %s{describeError error}. A SimulatedUnixPlatform that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with SimulatedUnixPlatform.create instead."

    /// Whose `<errno.h>` numbering this platform reports, for the errors where
    /// the two Unixes disagree.
    ///
    /// This is the choice `UnixError.toRawErrnoUnder` takes as its first argument, and
    /// it is what lets an `ELOOP` reach a process at all: raw 40 is `ELOOP` on
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
    /// 17 is `SIGCHLD` on Linux and `SIGSTOP` on Darwin, so a client that
    /// asks for `SIGCHLD` must be handed 17 on the one and 20 on the other,
    /// and one that hands 17 back must be told it cannot catch it on Darwin
    /// alone. `Signal.toRawSignoUnder` and its siblings take the
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
        match kernel platform with
        | SimulatedUnixKernel.Linux version ->
            // Commit 1fb0e471611d ("net: remove one stac/clac pair from
            // move_addr_to_user()"), first released in 6.18, moved the length's
            // store ahead of the address's copy.
            let storesLengthFirst =
                {
                    Major = 6u
                    Minor = 18u
                    Patch = 0u
                }

            if version >= storesLengthFirst then
                GetSockNameFaultLength.AlreadyReported
            else
                GetSockNameFaultLength.Untouched
        | SimulatedUnixKernel.Darwin -> GetSockNameFaultLength.Untouched

    /// The number every descriptor this kernel hands out lies below: the soft
    /// `RLIMIT_NOFILE` it assumes the process has at least. A call that would
    /// put a descriptor at or above it is refused (`DescriptorLimitRefusal`),
    /// since what it answers depends on a limit this kernel does not model.
    ///
    /// It is the flavour's default soft limit at process start, so a process
    /// is below it only if its parent lowered the limit.
    let descriptorBound (platform : SimulatedUnixPlatform) : int =
        // Measured by `rlimit-nofile.c`: 1024 on Linux, the kernel's own default
        // (INR_OPEN_CUR, read as the first process of a booted 6.12 x86-64
        // kernel) and what PAM's login path gives uid 1000 on 6.18.5 aarch64;
        // 256 on Darwin, what launchd gives a job on 27.0.0.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 1024
        | SimulatedUnixFlavour.Darwin -> 256

    /// Whether the socket `accept(2)` hands back inherits `O_NONBLOCK` from the
    /// listening descriptor.
    ///
    /// The classic BSD/POSIX divergence, measured 2026-08-28 with
    /// `docs/plans/2026-08-23-posix-kernel-extraction/accept-inherits-nonblock.c`:
    /// on Linux 6.18.5 a non-blocking listener yields a *blocking* accepted
    /// socket, and on Darwin 25.6.0 a non-blocking one. Blocking listeners yield
    /// blocking sockets on both.
    ///
    /// This is the kernel's answer. A client whose own sockets expect one
    /// answer everywhere normalises it after `accept` returns, and that
    /// normalisation belongs to the client rather than here.
    let acceptedSocketInheritsNonBlocking (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// Whether this platform's `stat(2)` reports a creation time.
    ///
    /// Darwin's `struct stat` has `st_birthtimespec`; Linux's has no such
    /// field, and only `statx(2)` reports one (`stx_btime`). The birth time is
    /// a fact about the inode on both, and this governs only whether `stat`
    /// tells a caller.
    let reportsBirthTime (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// Whether this platform's `stat(2)` has an `st_flags` field, holding the
    /// BSD file flags `chflags(2)` sets.
    ///
    /// Darwin's does, and Linux's `struct stat` has no such field.
    let reportsFileFlags (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> false
        | SimulatedUnixFlavour.Darwin -> true

    /// The largest `st_nlink` this platform's `stat(2)` reports, or `None`
    /// where no count this kernel can hold reaches the field's limit.
    ///
    /// Darwin's `nlink_t` is 16 bits wide, and a larger count is reported as
    /// 65535 rather than wrapped. Linux's is at least 32 bits wide, as wide as
    /// the count the kernel keeps.
    let linkCountCeiling (platform : SimulatedUnixPlatform) : int64 option =
        match flavour platform with
        // Measured 2026-10-02 by `stat-nlink-limit.c` on Linux 6.18.5: a tmpfs
        // directory with 70000 subdirectories reports 70002.
        | SimulatedUnixFlavour.Linux -> None
        // Measured 2026-10-02 by `stat-nlink-limit.c` on Darwin 27.0: an APFS
        // directory reports 2 plus its names up to 65535, and 65535 from
        // there to 70000 names. A directory is the only kind whose count this
        // kernel lets grow that far.
        | SimulatedUnixFlavour.Darwin -> Some 65535L

    /// Whether this platform's libc provides <c>posix_fadvise(2)</c> at all.
    ///
    /// Measured, not read from a feature test: macOS 26.6's libc exports no such
    /// symbol, so a program that called it would not link, and there is no
    /// answer for a caller to compare against. Linux 6.18.5 provides it.
    /// <c>UnixDescriptor.posixFadvise</c> refuses on a platform without it.
    ///
    /// Darwin's nearest equivalent is the <c>F_RDADVISE</c> fcntl, which takes a
    /// different argument shape and is not modelled: the two are not
    /// interchangeable, so this is a genuine "no such call" rather than a
    /// renaming.
    let providesPosixFadvise (platform : SimulatedUnixPlatform) : bool =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> true
        | SimulatedUnixFlavour.Darwin -> false

    /// The permission bits this platform gives a symbolic link created by a
    /// process whose umask is `umask`: 0o777 on Linux whatever the umask, and
    /// 0o777 less the umask on Darwin, as it does for a regular file.
    let internal symlinkCreationPermissions
        (platform : SimulatedUnixPlatform)
        (umask : PermissionBits)
        : PermissionBits
        =
        // Measured by `link-symlink.c` (SYMMODE) on Linux 6.18.5 and Darwin
        // 27.0, under umasks 000, 022, 077, 0700 and 0777.
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PermissionBits.parseOrFail "SimulatedUnixPlatform.symlinkCreationPermissions" 0o777
        | SimulatedUnixFlavour.Darwin ->
            PermissionBits.parseOrFail
                "SimulatedUnixPlatform.symlinkCreationPermissions"
                (0o777 &&& ~~~(PermissionBits.toInt umask))

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
    /// `O_TRUNC` and a no-op `ftruncate` alike. Linux applies the same rule it
    /// applies to a write.
    /// **Darwin strips nothing at all**, and that is isolated rather than
    /// inferred: in one process, on one file, a one-byte `write` takes `04755` to
    /// `00755` there while `ftruncate` leaves it `04755`.
    let setIdBitsOnTruncation (platform : SimulatedUnixPlatform) : SetIdBitsOnTruncation =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetIdBitsOnTruncation.Strip
        | SimulatedUnixFlavour.Darwin -> SetIdBitsOnTruncation.Preserve

    /// What this platform's `chmod(2)` and `fchmod(2)` do for a privileged
    /// caller.
    ///
    /// Everything else about a mode change is unanimous: who may make one, the
    /// `S_ISGID` an owner outside the inode's group loses, the bits above
    /// `0o7777` that are ignored, and the `ctime` that moves.
    let privilegedModeChange (platform : SimulatedUnixPlatform) : PrivilegedModeChange =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/chmod-rules.c`:
        // on Linux 6.18.5 root sets all 4096 modes exactly, on a file and on a
        // directory, whether it owns the inode or not and whether it is in the
        // inode's group or not. Darwin 27.0 was measured at uid 501 only.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PrivilegedModeChange.SetsRequestedBits
        | SimulatedUnixFlavour.Darwin -> PrivilegedModeChange.Unmeasured

    /// What this platform's `fchmodat(2)` does with `AT_SYMLINK_NOFOLLOW` when
    /// the path names a symbolic link.
    let symlinkModeChange (platform : SimulatedUnixPlatform) : SymlinkModeChange =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/chmod-chown-at.c`
        // (NOFOLLOW, LINKMODE, LINKTIMES). Linux 6.18.5, through glibc and
        // through the fchmodat2 syscall alike: EOPNOTSUPP for a link to a
        // file, to a directory, dangling and to itself, as root and as uid
        // 1000, for all 4096 modes, on the caller's own link and on another
        // user's, the mode and every timestamp left as they were. Darwin 27.0
        // at uid 501: all 4096 modes set on its own link as `chmod` would set
        // them on a file, in the link's group and out of it; EPERM on root's;
        // the link's ctime moves and its target's timestamps do not.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SymlinkModeChange.NotSupported
        | SimulatedUnixFlavour.Darwin -> SymlinkModeChange.ChangesLink

    /// What a privileged caller is granted when it asks to execute something
    /// that is not a directory.
    let privilegedExecution (platform : SimulatedUnixPlatform) : PrivilegedExecution =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/access-rules.c`:
        // on Linux 6.18.5 root's `access(X_OK)` on a regular file is granted
        // exactly when some execute bit is set, over all 4096 modes, owning it
        // or not, in its group or not. Darwin 27.0 was measured at uid 501 only.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PrivilegedExecution.NeedsAnExecuteBit
        | SimulatedUnixFlavour.Darwin -> PrivilegedExecution.Unmeasured

    /// Who may change an inode's owner and group on this platform, and which
    /// set-ID bits a change clears.
    let ownerChangeRule (platform : SimulatedUnixPlatform) : OwnerChangeRule =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/chown-rules.c`
        // against exactly these two rules: Linux 6.18.5 (ext4 and tmpfs) for
        // every caller standing over every mode and every kind of ID named;
        // Darwin 27.0 at uid 501 for its owner over every mode it could set,
        // and for a non-owner on other users' inodes. Its output is beside it.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> OwnerChangeRule.ClearsSetIdFromNonDirectories
        | SimulatedUnixFlavour.Darwin -> OwnerChangeRule.ClearsSetIdWhenAnIdIsNamed

    /// Whether this platform's content-changing `write(2)` clears `S_ISGID` on a
    /// file that is not group-executable.
    ///
    /// The only thing about a write's effect on the mode that the two Unixes
    /// disagree about: `S_ISUID` goes on both whatever the execute bits say, and
    /// the sticky bit is left alone by both. On Linux the answer also depends on
    /// whether the writer is in the file's group, which is why the rule takes a
    /// `Standing`. So this is a lone value rather than
    /// a `CreatingOpenRules`-shaped record, for the reason
    /// `setIdBitsOnTruncation` above gives.
    ///
    /// Measured non-root on macOS 26.6 and Linux 6.18.5, one byte written over
    /// the front of a four-byte file, and since then on Linux by writers
    /// standing in every relation to the file. Linux applies to a write the same
    /// rule it applies to a
    /// truncation, and **Darwin does not** — there a write strips `02644` to
    /// `00644` while an `ftruncate` on the same file leaves the whole mode alone,
    /// which is why the two rules are separate values rather than one.
    ///
    /// The file must be handed to a group the caller belongs to before `chmod`,
    /// or the kernel drops `S_ISGID` silently and the measurement reads as
    /// agreement.
    let setGroupIdOnWrite (platform : SimulatedUnixPlatform) : SetGroupIdOnWrite =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> SetGroupIdOnWrite.StripWhenGroupExecutableOrWriterOutsideGroup
        | SimulatedUnixFlavour.Darwin -> SetGroupIdOnWrite.StripAlways

    /// How this platform's `*at` syscalls find the directory a path starts
    /// from; see `StartingPointRules`.
    ///
    /// Measured by `at-dirfd.c` on Linux 6.18.5 and Darwin 27.0, over nineteen
    /// calls and thirteen kinds of `dirfd`. Linux answers the empty path
    /// ENOENT before it looks `dirfd` up, and ENOTDIR for any open descriptor
    /// that is not a directory. Darwin looks `dirfd` up first, and answers
    /// ENOTDIR for a regular file or a device but ENOTSUP for a pipe, a socket
    /// or a kqueue.
    let startingPointRules (platform : SimulatedUnixPlatform) : StartingPointRules =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                EmptyPath = EmptyPathRule.NoSuchEntryBeforeDescriptor
                NonDirectory = NonDirectoryDescriptorRule.NotADirectory
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                EmptyPath = EmptyPathRule.NoSuchEntryAfterDescriptor
                NonDirectory = NonDirectoryDescriptorRule.NotSupportedOffTheFileSystem
            }

    /// How this platform's `link(2)` and `linkat(2)` differ from the other's;
    /// see `LinkRules`.
    let linkRules (platform : SimulatedUnixPlatform) : LinkRules =
        // Measured by `link-symlink.c` (LINKSRC, LINKDST) and `link-rules.c`
        // (ORDER, DESTORDER) on Linux 6.18.5 and Darwin 27.0.
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                PlainLinkSource = SymlinkPolicy.NoFollowFinal
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                DirectorySource = DirectorySourceRefusal.Last
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                PlainLinkSource = SymlinkPolicy.Follow
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                DirectorySource = DirectorySourceRefusal.BeforeDestination
            }

    /// How this platform's `symlink(2)` differs from the other's; see
    /// `SymlinkRules`.
    let symlinkRules (platform : SimulatedUnixPlatform) : SymlinkRules =
        // Measured by `link-symlink.c` (SYMLINK, SYMORDER) on Linux 6.18.5
        // and Darwin 27.0.
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Ignore
                EmptyTarget = EmptySymlinkTarget.NoSuchEntry
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                EmptyTarget = EmptySymlinkTarget.Accepted
            }

    /// How this platform's `mknod(2)` treats the type of node it is asked
    /// for; see `MkNodRules`.
    let mkNodRules (platform : SimulatedUnixPlatform) : MkNodRules =
        // Measured by `mknodat-rules.c` (TYPE, ORDER, PATH, PATHTYPE) on
        // Linux 6.18.5, root and uid 1000, ext4 and tmpfs, and Darwin 27.0,
        // uid 501. Linux's walk is its `mkdir`'s and `symlink`'s
        // (`filename_create`): "f/", "dang/", "cyc/", "lf/" and "ld/" are
        // EEXIST and a free "nx/" is ENOENT.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> MkNodRules.TypeBeforePath TrailingSeparatorPolicy.Ignore
        | SimulatedUnixFlavour.Darwin -> MkNodRules.PrivilegeBeforePath

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
                ScreensStickyDirectoryEntries = true
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                TrailingSeparator = TrailingSeparatorPolicy.Demand
                RefusesExistingDirectory = false
                RootNavigation = Some UnixError.EEXIST
                ModeMask = PermissionBits.parseOrFail "SimulatedUnixPlatform.creatingOpenRules" 0o0777
                ScreensStickyDirectoryEntries = false
            }

    /// Where the group of an inode this platform's `open(O_CREAT)` or `mkdir(2)`
    /// creates comes from.
    ///
    /// A mount fact as well as a kernel one on Linux, whose `grpid` mount
    /// option makes every directory behave as if set-group-ID; this library
    /// models no such mount.
    let newInodeGroupRule (platform : SimulatedUnixPlatform) : NewInodeGroupRule =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> NewInodeGroupRule.CreatorsUnlessParentSetGroupId
        | SimulatedUnixFlavour.Darwin -> NewInodeGroupRule.Parents

    /// Which groups this platform's `getgroups(2)` reports for a process's
    /// credentials.
    let groupListReport (platform : SimulatedUnixPlatform) : GroupListReport =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> GroupListReport.SortedSupplementaryGroups
        | SimulatedUnixFlavour.Darwin -> GroupListReport.Unmeasured

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

    /// The bits of its argument this platform's `umask(2)` keeps as the
    /// process's file-mode creation mask: 0o777 on Linux, and all twelve
    /// permission bits, 0o7777, on Darwin. Every other bit of the argument is
    /// ignored on both.
    ///
    /// The stored mask is applied in full wherever a mode is masked. The
    /// special bits Darwin keeps are
    /// invisible to its `open(2)` and `mkdir(2)`, which drop those bits from the
    /// mode first, but `umask(2)` reports them.
    let umaskStoredBits (platform : SimulatedUnixPlatform) : PermissionBits =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/umask-width.c`
        // on Linux 6.18.5 (aarch64, as root and as uid 1000) and Darwin 27.0
        // (uid 501): every 12-bit argument, and each of bits 12 to 31 over four
        // low words, through libc's `umask` and through the raw syscall alike,
        // reads back as the argument ANDed with this, with no mismatch.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "SimulatedUnixPlatform.umaskStoredBits" 0o0777
        | SimulatedUnixFlavour.Darwin -> PermissionBits.parseOrFail "SimulatedUnixPlatform.umaskStoredBits" 0o7777

    /// Everything this platform's `unlink(2)` does differently. See
    /// `UnlinkRules`, whose one field this picks; the rest of the divergence
    /// follows from the flavour alone.
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
    /// whose two fields this picks; the ordering half of the divergence
    /// follows from the flavour alone.
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
    /// — which is most of the divergence — follows from the flavour alone.
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
    /// *Where* it screens is the machine's `UserBufferCheck`, not a property
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

    /// What this platform's `read(2)` family does with a count larger than one
    /// call moves. See `TransferCountLimit`.
    let transferCountLimit (platform : SimulatedUnixPlatform) : TransferCountLimit =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            // `MAX_RW_COUNT`: `INT_MAX` rounded down to a whole page. Measured
            // 0x7FFFF000 with 4 KiB pages, the only size a Linux platform
            // admits, on 6.18.5 aarch64 for read, pread, write and pwrite (and
            // for `getrandom`, on 6.12 x86-64 too): every count from 0x7FFFF000
            // to 2^48 moved exactly 0x7FFFF000 bytes. The range check sees the
            // count before this shortening, and so does the check on the file
            // position; docs/plans/2026-08-23-posix-kernel-extraction/
            // transfer-counts.c and transfer-counts-position.c.
            let pageBytes = SimulatedPageSize.bytes (pageSize platform)
            TransferCountLimit.Shortened (System.Int32.MaxValue &&& ~~~(pageBytes - 1))
        | SimulatedUnixFlavour.Darwin ->
            // Measured on 27.0.0 arm64: every count above INT_MAX is EINVAL,
            // ahead of a descriptor that is not open, through the raw syscall as
            // through libc; INT_MAX itself moves INT_MAX bytes in one call.
            TransferCountLimit.Refused System.Int32.MaxValue

    /// How this platform's `readlink(2)` treats a buffer size that is not
    /// positive, measured on both (`docs/probes/readlink/capacity.py`):
    ///
    /// * Linux refuses `bufsiz <= 0` with EINVAL before looking at the path
    ///   (`do_readlinkat` checks it first), so a missing path is EINVAL too.
    /// * Darwin takes a `size_t`, so a negative `int` arrives as a size past
    ///   `INT_MAX`, which it refuses with EINVAL before resolving; a size of
    ///   zero resolves the path, and a symbolic link answers 0 with the buffer
    ///   never consulted -- a null buffer is accepted.
    ///
    /// A positive size is admitted on both.
    let internal readlinkCapacity (platform : SimulatedUnixPlatform) (capacity : int) : ReadLinkCapacityVerdict =
        if capacity > 0 then
            ReadLinkCapacityVerdict.Admit
        else

        match flavour platform with
        | SimulatedUnixFlavour.Linux -> ReadLinkCapacityVerdict.Refuse UnixError.EINVAL
        | SimulatedUnixFlavour.Darwin ->
            if capacity = 0 then
                ReadLinkCapacityVerdict.ReportNothing
            else
                ReadLinkCapacityVerdict.Refuse UnixError.EINVAL

    /// What this platform's `readlinkat(2)` makes of an empty path, measured
    /// on both (`at-dirfd.c`, `readlinkat-empty-path.c`):
    ///
    /// * Linux names the object `dirfd` names: a symbolic link is read, and
    ///   anything else, the current directory included, is ENOENT, after a
    ///   `dirfd` that names nothing is EBADF.
    /// * Darwin walks it as every `*at` call does (`StartingPointRules`).
    let readlinkEmptyPath (platform : SimulatedUnixPlatform) : EmptyPathMeaning =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> EmptyPathMeaning.NamesStartingPoint
        | SimulatedUnixFlavour.Darwin -> EmptyPathMeaning.Walked

    /// Whether this platform's `readlink(2)` consults the link's own
    /// permission bits: Linux never does, and Darwin refuses a caller the
    /// read bit of the triple its standing selects.
    let linkReadRule (platform : SimulatedUnixPlatform) : LinkReadRule =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/readlink-mode.c`
        // on Linux 6.18.5 (root and uid 1000, links whose modes debugfs set
        // on ext4) and Darwin 27.0 (uid 501). Darwin's root was not measured.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> LinkReadRule.ModeIgnored
        | SimulatedUnixFlavour.Darwin -> LinkReadRule.ReadBitOfSelectedTriple

    /// The bounds this platform's kernel puts on path resolution.
    ///
    /// The numbers are measured facts about real kernels, which is why they are
    /// derived from the flavour rather than configured: a host that could set
    /// them could describe a Unix that does not exist, and a process would then
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
    /// UTF-16 units, and APFS resolves it where ext4 refuses it. Darwin measures
    /// a name that is not valid UTF-8 in bytes instead, against 765.
    let pathLimits (platform : SimulatedUnixPlatform) : PathLimits =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PathLimits.create 40 4096 (NameLengthLimit.Bytes 255) SpliceLengthRecheck.NoRecheck
        | SimulatedUnixFlavour.Darwin ->
            PathLimits.create 32 1024 (NameLengthLimit.Utf16CodeUnitsOrBytes (255, 765)) SpliceLengthRecheck.Recheck

    /// Which names this platform's filesystem will bind.
    ///
    /// Like `pathLimits`, this is really a property of the mounted filesystem
    /// rather than of the kernel. It lives here because this library models one
    /// filesystem per flavour; a second filesystem on one flavour is what would
    /// make it configuration instead.
    let bindableEntryNames (platform : SimulatedUnixPlatform) : BindableEntryNames =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> BindableEntryNames.AnyBytes
        | SimulatedUnixFlavour.Darwin -> BindableEntryNames.StrictUtf8

    /// `sizeof(struct sockaddr_storage)`: the size of the largest socket address
    /// any Unix we model can hand back.
    ///
    /// Takes no flavour: both families *define* the constant in their headers
    /// rather than computing it (`_SS_MAXSIZE` on Darwin, `_SS_SIZE` in glibc's
    /// generic `bits/sockaddr.h`) and derive the padding members from it, so the
    /// value is invariant of pointer width as well as agreed between the two —
    /// both descend from RFC 2553's sample definition. Measured 128 on macOS
    /// arm64 and on Linux alike. Make it a function of the flavour on the day one
    /// of them disagrees.
    let maximumSocketAddressSize : int = 128

    /// `sizeof(struct sockaddr_in)`: 16 on both flavours.
    let internetSocketAddressSize : int = 16

    /// `sizeof(struct sockaddr_in6)`: 28 on both flavours. Measured.
    let internetV6SocketAddressSize : int = 28

    /// The shortest `struct sockaddr_in6` an IPv6 socket's `bind(2)` and
    /// `connect(2)` take: 24 bytes, `SIN6_LEN_RFC2133`, which stops short of
    /// `sin6_scope_id`. Measured on Linux, which answers `EINVAL` below it;
    /// Darwin's lengths below it read a truncated address, and this library
    /// does not answer them.
    let minimumInternetV6SocketAddressLength : int = 24

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
    let internal firstBindFault (platform : SimulatedUnixPlatform) (faults : Set<BindFault>) : BindFault option =
        bindFaultOrder platform |> List.tryFind (fun fault -> Set.contains fault faults)

    /// The order an IPv6 socket's `bind(2)` reports its faults in, which is
    /// neither flavour's IPv4 order. Measured pairwise
    /// (`docs/probes/dual-mode/dual-mode.c`, B5 and F) with a v4-mapped
    /// address: Linux judges the length, then the family, then an
    /// unprivileged caller's port, then `IPV6_V6ONLY`, and only then whether
    /// the socket is bound -- so a bound socket's rebind to an address it does
    /// not hold is `EINVAL`, where an IPv4 socket's is `EADDRNOTAVAIL`. Darwin
    /// judges the family (with a broadcast or multicast address), then whether
    /// the socket is bound, then `IPV6_V6ONLY` and the address, then the port.
    /// Darwin's length is not among them: this library does not answer the
    /// lengths it would reject.
    let bindV6FaultOrder (platform : SimulatedUnixPlatform) : BindFault list =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            [
                BindFault.Length
                BindFault.Family
                BindFault.PrivilegedPort
                BindFault.Ipv6Only
                BindFault.AlreadyBound
                BindFault.AddressNotLocal
                BindFault.AddressInUse
            ]
        | SimulatedUnixFlavour.Darwin ->
            [
                BindFault.Family
                BindFault.AlreadyBound
                BindFault.Ipv6Only
                BindFault.AddressNotLocal
                BindFault.PrivilegedPort
                BindFault.AddressInUse
            ]

    /// What an IPv6 socket's `bind(2)` answers for `BindFault.Ipv6Only`: a
    /// socket with `IPV6_V6ONLY` on asked for a v4-mapped address. `EINVAL`
    /// on Linux and `EADDRNOTAVAIL` on Darwin. Measured.
    let ipv6OnlyBindError (platform : SimulatedUnixPlatform) : UnixError =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> UnixError.EINVAL
        | SimulatedUnixFlavour.Darwin -> UnixError.EADDRNOTAVAIL

    /// What an IPv6 socket's `connect(2)` answers when `IPV6_V6ONLY` is on
    /// and the destination is v4-mapped: `ENETUNREACH` on Linux and
    /// `EAFNOSUPPORT` on Darwin, binding nothing on either. Measured.
    let ipv6OnlyConnectError (platform : SimulatedUnixPlatform) : UnixError =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> UnixError.ENETUNREACH
        | SimulatedUnixFlavour.Darwin -> UnixError.EAFNOSUPPORT

    /// The greatest `socketAddressLen` Darwin's `bind(2)` will consider at all.
    /// Above it the answer is `ENAMETOOLONG` rather than `EINVAL`; measured, 255
    /// is `EINVAL` and 256 is `ENAMETOOLONG`. Linux has no such threshold.
    let maximumDarwinSocketAddressLength : int = 255

    /// How long `bind(2)` and `connect(2)` insist a `struct sockaddr_in` argument is.
    ///
    /// Measured, and not the same shape on the two: Linux accepts any length from
    /// the family's own `sizeof` up to `sizeof(struct sockaddr_storage)` — 16
    /// through 128 inclusive for IPv4, with 129 the least rejected — while Darwin
    /// insists on exactly 16 and answers `EINVAL` for every value from 17 to 255.
    ///
    /// `declared` is the caller's 32-bit length exactly as passed. Linux reads it
    /// as an `int`, so a word at or above 2^31 is a negative length, which it
    /// rejects with `EINVAL` before the copy as it does an over-long one; Darwin
    /// reads it as the `socklen_t` it is, so the same word is a length past its
    /// threshold.
    let internal bindAddressLength
        (platform : SimulatedUnixPlatform)
        (exactSize : int)
        (declared : uint32)
        : BindLengthVerdict
        =
        match flavour platform with
        | SimulatedUnixFlavour.Linux ->
            // `move_addr_to_kernel`: `if (ulen < 0 || ulen > sizeof(struct
            // sockaddr_storage)) return -EINVAL;`, measured at every word the
            // probe swept (`socket-address-length.c`).
            let declared = int declared

            if declared < 0 || declared > maximumSocketAddressSize then
                BindLengthVerdict.RejectedBeforeCopy UnixError.EINVAL
            elif declared >= exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid
        | SimulatedUnixFlavour.Darwin ->
            if declared > uint32 maximumDarwinSocketAddressLength then
                BindLengthVerdict.RejectedBeforeCopy UnixError.ENAMETOOLONG
            elif declared = uint32 exactSize then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid

    /// How long `bind(2)` and `connect(2)` on an IPv6 socket insist its
    /// `struct sockaddr_in6` is. Linux takes 24 through 128 and answers
    /// `EINVAL` outside, the upper bound before the copy; Darwin takes 24
    /// through 255 and answers `ENAMETOOLONG` above, before the copy. Measured
    /// (`docs/probes/dual-mode/dual-mode.c`, A14 and B5). `Invalid` on Darwin
    /// is a length this library refuses before any ladder reads it.
    let internal internetV6AddressLength (platform : SimulatedUnixPlatform) (declared : uint32) : BindLengthVerdict =
        match bindAddressLength platform minimumInternetV6SocketAddressLength declared with
        | BindLengthVerdict.RejectedBeforeCopy error -> BindLengthVerdict.RejectedBeforeCopy error
        | BindLengthVerdict.Accepted
        | BindLengthVerdict.Invalid ->
            if declared >= uint32 minimumInternetV6SocketAddressLength then
                BindLengthVerdict.Accepted
            else
                BindLengthVerdict.Invalid

    /// Is this the all-ones broadcast address, or a multicast one
    /// (`224.0.0.0/4`)?
    ///
    /// What `bind(2)` does with one is `bindGroupAddressRule`'s. **This library
    /// refuses any bind of one that would succeed**, rather than recording it:
    /// it models no group membership and no interface to receive or broadcast
    /// on, so such a binding would become a lie the moment a transfer landed.
    /// Every bind of one that fails has its measured errno.
    let internal isBroadcastOrMulticast (address : uint32) : bool =
        address = System.UInt32.MaxValue || (address >>> 28) = 0xEu

    /// May a socket bind to this address, given the addresses this machine holds?
    ///
    /// The wildcard always binds. Beyond that the flavours read the same list
    /// differently, which is measured rather than inferred: `127.9.9.9` binds on
    /// Linux and is `EADDRNOTAVAIL` on Darwin, because Linux treats every address
    /// inside a local prefix as assigned while Darwin assigns loopback exactly
    /// one address.
    ///
    /// Says nothing about broadcast and multicast addresses, which each flavour
    /// rules on apart from its address list: see `bindGroupAddressRule`.
    let internal isBindableAddress
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

    /// What this platform's `bind(2)` makes of `address` on a socket of `kind`,
    /// if it is the broadcast address or a multicast one; `None` for any other.
    ///
    /// Measured (`sockaddr-bind-ladder.c`, M and Z): Linux binds both, on
    /// either kind of socket. Darwin binds a multicast address on a datagram
    /// socket and answers `EADDRNOTAVAIL` for the broadcast address there; on a
    /// stream socket it answers `EAFNOSUPPORT` for both, before it asks whether
    /// the socket is already bound. None of this depends on the addresses the
    /// machine holds, so a client that lists such an address cannot change it.
    let internal bindGroupAddressRule
        (platform : SimulatedUnixPlatform)
        (kind : SocketKind)
        (address : uint32)
        : BindGroupAddressRule option
        =
        if not (isBroadcastOrMulticast address) then
            None
        else

        match flavour platform, kind with
        | SimulatedUnixFlavour.Linux, _ -> Some BindGroupAddressRule.Accepted
        | SimulatedUnixFlavour.Darwin, SocketKind.Stream -> Some BindGroupAddressRule.RejectedWithTheFamily
        | SimulatedUnixFlavour.Darwin, _ when address = System.UInt32.MaxValue -> Some BindGroupAddressRule.NotLocal
        | SimulatedUnixFlavour.Darwin, _ -> Some BindGroupAddressRule.Accepted

    /// Whether `bind(2)` rules on the address itself, on a socket of `kind`, as
    /// opposed to on the length, the family, or another socket: `EADDRNOTAVAIL`,
    /// ranked against the other faults at `BindFault.AddressNotLocal`.
    let internal bindAddressFaults
        (platform : SimulatedUnixPlatform)
        (kind : SocketKind)
        (localAddresses : uint32 list)
        (localRoutes : Ipv4Prefix list)
        (address : uint32)
        : bool
        =
        match bindGroupAddressRule platform kind address with
        | Some BindGroupAddressRule.NotLocal -> true
        | Some BindGroupAddressRule.Accepted
        | Some BindGroupAddressRule.RejectedWithTheFamily -> false
        | None -> not (isBindableAddress platform localAddresses localRoutes address)

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
    /// With the flag absent from the candidate, the two agree and refuse.
    ///
    /// Each socket's flag is read as it stands when the question is asked, not
    /// as it stood when that socket was bound.
    ///
    /// The same relation answers `listen(2)`, which is measured rather than
    /// assumed: on Linux two reuse-carrying sockets may share an endpoint until
    /// one listens, and the *second* `listen` is then EADDRINUSE — exactly what
    /// this says when the other socket is already listening. Darwin never refuses
    /// a listen, and never lets the pair coexist in the first place.
    let internal bindConflict
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
            | SocketPhase.Refused _
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

    /// `AF_INET`, in the platform's own numbering. 2 on both, and on essentially
    /// every Unix — it is one of the handful of `AF_*` values that predate the
    /// BSD/Linux split and never moved.
    ///
    /// Exposed alongside `internetV6AddressFamily` because a caller reading a
    /// `sockaddr` switches on the raw `sa_family` in the blob, in the platform's
    /// own numbering.
    let internetAddressFamily : int = 2

    /// Ports a process may bind only as root.
    ///
    /// Measured as 1024 on both: binding 1023 is `EACCES` for an unprivileged
    /// caller and 1024 succeeds. A constant rather than a function of the
    /// platform because the two agree, and not configuration though Linux does
    /// expose it as `ip_unprivileged_port_start` -- nothing needs to vary it
    /// yet, and a knob with no consumer is a knob no test covers.
    let privilegedPortCeiling : uint16 = 1024us

    /// The most supplementary groups a process on this platform can hold:
    /// `NGROUPS_MAX`, 65536 on Linux and 16 on Darwin.
    let supplementaryGroupLimit (platform : SimulatedUnixPlatform) : int =
        // Measured by `docs/plans/2026-08-23-posix-kernel-extraction/credentials.c`
        // as `setgroups(2)`'s own boundary, on Linux 6.18.5 as root: 65536 groups
        // succeed and 65537 are EINVAL. On Darwin 27.0 at uid 501, 17 groups are
        // EINVAL while 16 get as far as the privilege check (EPERM), so the count
        // is checked first and 16 passes it. `sysconf(_SC_NGROUPS_MAX)` agrees on
        // both.
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 65536
        | SimulatedUnixFlavour.Darwin -> 16

    /// `AF_INET6`, in the platform's own numbering, which unlike `AF_INET` the two
    /// families disagree about: 10 on Linux against 30 on Darwin. Measured.
    let internetV6AddressFamily (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 10
        | SimulatedUnixFlavour.Darwin -> 30

    /// `SOL_SOCKET`, the `level` at which `setsockopt(2)` and `getsockopt(2)`
    /// name the options every socket has whatever its protocol, in the
    /// platform's own numbering: 1 on Linux, `0xffff` on Darwin. Measured.
    let socketOptionLevel (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 1
        | SimulatedUnixFlavour.Darwin -> 0xffff

    /// `SO_REUSEADDR`, at `socketOptionLevel`, in the platform's own numbering:
    /// 2 on Linux, 4 on Darwin. Measured.
    let reuseAddressOption (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 2
        | SimulatedUnixFlavour.Darwin -> 4

    /// `SO_ERROR`, at `socketOptionLevel`, in the platform's own numbering:
    /// 4 on Linux, `0x1007` on Darwin. Measured.
    let socketErrorOption (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 4
        | SimulatedUnixFlavour.Darwin -> 0x1007

    /// `IPPROTO_TCP`, the `level` at which `setsockopt(2)` and `getsockopt(2)`
    /// name TCP's own options: 6 on both. Measured.
    let tcpOptionLevel (_ : SimulatedUnixPlatform) : int = 6

    /// `TCP_NODELAY`, at `tcpOptionLevel`: 1 on both. Measured.
    let noDelayOption (_ : SimulatedUnixPlatform) : int = 1

    /// `IPPROTO_IPV6`, the `level` at which `setsockopt(2)` and `getsockopt(2)`
    /// name IPv6's options: 41 on both. Measured.
    let ipv6OptionLevel (_ : SimulatedUnixPlatform) : int = 41

    /// `IPV6_V6ONLY`, at `ipv6OptionLevel`, in the platform's own numbering: 26
    /// on Linux, 27 on Darwin. Measured.
    let ipv6OnlyOption (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 26
        | SimulatedUnixFlavour.Darwin -> 27

    /// `SO_LINGER`, at `socketOptionLevel`, in the platform's own numbering: 13
    /// on Linux, `0x80` on Darwin. Measured. Its `l_linger` is in seconds on
    /// Linux and in hundredths of a second on Darwin; see `lingerSecondsOption`.
    let lingerOption (platform : SimulatedUnixPlatform) : int =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> 13
        | SimulatedUnixFlavour.Darwin -> 0x80

    /// Darwin's `SO_LINGER_SEC`, at `socketOptionLevel`: `SO_LINGER` with its
    /// `l_linger` in seconds rather than in hundredths of one. `0x1080`.
    /// Measured. Linux has no such option.
    let lingerSecondsOption (platform : SimulatedUnixPlatform) : int option =
        match flavour platform with
        | SimulatedUnixFlavour.Linux -> None
        | SimulatedUnixFlavour.Darwin -> Some 0x1080

    /// Whether a multi-byte integer the process stores — `sa_family` in a
    /// `struct sockaddr`, a socket option's `int` — has its least significant
    /// byte first.
    ///
    /// Both architectures this library models are little-endian; the match is
    /// here so that one which is not must say so.
    let private machineIsLittleEndian (platform : SimulatedUnixPlatform) : bool =
        match architecture platform with
        | SimulatedUnixArchitecture.X64
        | SimulatedUnixArchitecture.Arm64 -> true

    /// `sa_family`, in this platform's own `AF_*` numbering, from the bytes of
    /// the family field `sockaddrFamilyField` places: as many as it is wide, in
    /// the machine's own byte order.
    ///
    /// Any other number of bytes is not a family field, and is refused.
    let decodeSockaddrFamily (platform : SimulatedUnixPlatform) (field : ImmutableArray<byte>) : int =
        let width = SockaddrFamilyField.width (sockaddrFamilyField platform)

        if field.IsDefault || field.Length <> width then
            failwith
                $"SimulatedUnixPlatform.decodeSockaddrFamily: this platform's family field is %d{width} bytes wide, and the caller passed %d{(if field.IsDefault then 0 else field.Length)} (this is a bug in the caller)."

        match width with
        | 1 -> int field.[0]
        | _ ->
            if machineIsLittleEndian platform then
                int (BinaryPrimitives.ReadUInt16LittleEndian (field.AsSpan ()))
            else
                int (BinaryPrimitives.ReadUInt16BigEndian (field.AsSpan ()))

    /// The bytes of this platform's family field holding `family`, to be stored
    /// at `SockaddrFamilyField.offset`: in the machine's own byte order, and
    /// truncated to the field's width exactly as a C assignment through a
    /// `sa_family_t` truncates.
    let encodeSockaddrFamily (platform : SimulatedUnixPlatform) (family : int) : byte[] =
        match SockaddrFamilyField.width (sockaddrFamilyField platform) with
        | 1 -> [| byte family |]
        | width ->
            let bytes = Array.zeroCreate<byte> width

            if machineIsLittleEndian platform then
                BinaryPrimitives.WriteUInt16LittleEndian (System.Span<byte> bytes, uint16 family)
            else
                BinaryPrimitives.WriteUInt16BigEndian (System.Span<byte> bytes, uint16 family)

            bytes

    /// A C `int` as the process stores it: four bytes in the machine's own
    /// byte order. A socket option's value is one, or two side by side for
    /// `struct linger`.
    let encodeCInt (platform : SimulatedUnixPlatform) (value : int) : byte[] =
        let bytes = Array.zeroCreate<byte> 4

        if machineIsLittleEndian platform then
            BinaryPrimitives.WriteInt32LittleEndian (System.Span<byte> bytes, value)
        else
            BinaryPrimitives.WriteInt32BigEndian (System.Span<byte> bytes, value)

        bytes

    /// The C `int` in the four bytes of `bytes` from `offset`, as `encodeCInt`
    /// lays one out.
    let decodeCInt (platform : SimulatedUnixPlatform) (bytes : ImmutableArray<byte>) (offset : int) : int =
        if bytes.IsDefault || offset < 0 || bytes.Length < offset + 4 then
            failwith
                $"SimulatedUnixPlatform.decodeCInt: no four bytes at offset %d{offset} of %d{(if bytes.IsDefault then 0 else bytes.Length)} (this is a bug in the caller)."

        let span = bytes.AsSpan().Slice (offset, 4)

        if machineIsLittleEndian platform then
            BinaryPrimitives.ReadInt32LittleEndian span
        else
            BinaryPrimitives.ReadInt32BigEndian span

    /// What `copied`, every byte a `bind(2)` or `connect(2)` copied in, says
    /// when read as this platform's `struct sockaddr_in`.
    ///
    /// A field is present exactly when the copy reached all of it. Nothing else
    /// is read: measured on both flavours (`sockaddr-decoding.c`), neither kernel
    /// looks at `sin_zero` or at any byte past it, and Darwin ignores the
    /// `sa_len` byte, every one of its 256 values answering as the length
    /// argument says.
    let internal decodeInternetSockaddr
        (platform : SimulatedUnixPlatform)
        (copied : ImmutableArray<byte>)
        : CopiedInternetSockaddr
        =
        if copied.IsDefault then
            failwith
                "SimulatedUnixPlatform.decodeInternetSockaddr: copied is the default ImmutableArray, whose underlying array is null. That is not an empty copy; pass ImmutableArray<byte>.Empty."

        let family =
            let field = sockaddrFamilyField platform

            if SockaddrFamilyField.reachedBy field copied.Length then
                Some (
                    decodeSockaddrFamily
                        platform
                        (copied.Slice (SockaddrFamilyField.offset field, SockaddrFamilyField.width field))
                )
            else
                None

        let endpoint =
            if
                SockaddrField.reachedBy InternetSockaddr.port copied.Length
                && SockaddrField.reachedBy InternetSockaddr.address copied.Length
            then
                let span = copied.AsSpan ()

                let port =
                    BinaryPrimitives.ReadUInt16BigEndian (
                        span.Slice (InternetSockaddr.port.Offset, InternetSockaddr.port.Width)
                    )

                let address =
                    BinaryPrimitives.ReadUInt32BigEndian (
                        span.Slice (InternetSockaddr.address.Offset, InternetSockaddr.address.Width)
                    )

                Some (InternetEndpoint.ofParts address port)
            else
                None

        {
            Family = family
            Endpoint = endpoint
            ZeroFilledAddress =
                let word = Array.zeroCreate<byte> InternetSockaddr.address.Width
                let offset = InternetSockaddr.address.Offset

                for i in 0 .. word.Length - 1 do
                    if offset + i < copied.Length then
                        word.[i] <- copied.[offset + i]

                BinaryPrimitives.ReadUInt32BigEndian (System.ReadOnlySpan<byte> word)
        }

    /// `struct sockaddr_in` for `endpoint`, as this platform's kernel copies one
    /// out: the family, the port and the address, and on the flavours that have
    /// the field, the `sa_len` byte in front of them.
    ///
    /// The copy-*out* direction specifically. Measured: a Darwin `getsockname`
    /// on a bound socket reports `10 02 ...`, the leading `0x10` being the
    /// 16-byte length, so the kernel fills `sa_len` in even though the caller
    /// never wrote it. `SockaddrFamilyField.OneByteAtOffsetOne`
    /// describes the same byte travelling the other way, where it is a caller's
    /// own store; the two do not disagree.
    ///
    /// Answers the struct's full length for the platform; what a syscall copies
    /// out to a caller's shorter buffer is `copyOutInternetSockaddr`'s prefix
    /// of it.
    let encodeInternetSockaddr (platform : SimulatedUnixPlatform) (endpoint : InternetEndpoint) : byte[] =
        let realLength = internetSocketAddressSize
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
        let familyBytes = encodeSockaddrFamily platform internetAddressFamily
        familyBytes.CopyTo (blob, SockaddrFamilyField.offset field)

        match field with
        | SockaddrFamilyField.OneByteAtOffsetOne ->
            // Written only on the flavour that has the field -- on Linux those
            // two bytes are the family itself.
            blob.[0] <- byte realLength
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> ()

        blob

    /// What a `getsockname(2)` or `accept(2)` copies out to a caller that
    /// declared `declaredLength` bytes of room: the first
    /// `min(declaredLength, sizeof(struct sockaddr_in))` bytes of
    /// `encodeInternetSockaddr`. Measured on both flavours at every declared
    /// length 0..20 (`sockaddr-decoding.c`, T): the bytes written are always a
    /// prefix of the whole address, and a length past the struct writes the
    /// struct and no more.
    let internal copyOutInternetSockaddr
        (platform : SimulatedUnixPlatform)
        (endpoint : InternetEndpoint)
        (declaredLength : uint32)
        : ImmutableArray<byte>
        =
        let whole = encodeInternetSockaddr platform endpoint
        ImmutableArray.Create<byte> (whole, 0, int (min declaredLength (uint32 whole.Length)))

    /// What `copied`, every byte a `bind(2)` or `connect(2)` on an IPv6 socket
    /// copied in, says when read as this platform's `struct sockaddr_in6`.
    ///
    /// The family sits where `decodeInternetSockaddr` reads it; the port is
    /// network order at 2 and the address sixteen bytes at 8. A v4-mapped
    /// address, `::ffff:a.b.c.d`, is read as the IPv4 endpoint it maps.
    let internal decodeInternetV6Sockaddr
        (platform : SimulatedUnixPlatform)
        (copied : ImmutableArray<byte>)
        : CopiedInternetV6Sockaddr
        =
        if copied.IsDefault then
            failwith
                "SimulatedUnixPlatform.decodeInternetV6Sockaddr: copied is the default ImmutableArray, whose underlying array is null. That is not an empty copy; pass ImmutableArray<byte>.Empty."

        let family =
            let field = sockaddrFamilyField platform

            if SockaddrFamilyField.reachedBy field copied.Length then
                Some (
                    decodeSockaddrFamily
                        platform
                        (copied.Slice (SockaddrFamilyField.offset field, SockaddrFamilyField.width field))
                )
            else
                None

        let destination =
            if
                SockaddrField.reachedBy InternetV6Sockaddr.port copied.Length
                && SockaddrField.reachedBy InternetV6Sockaddr.address copied.Length
            then
                let span = copied.AsSpan ()

                let port =
                    BinaryPrimitives.ReadUInt16BigEndian (
                        span.Slice (InternetV6Sockaddr.port.Offset, InternetV6Sockaddr.port.Width)
                    )

                let address =
                    copied.Slice (InternetV6Sockaddr.address.Offset, InternetV6Sockaddr.address.Width)

                let mapped =
                    Seq.forall (fun i -> address.[i] = 0uy) (seq { 0..9 })
                    && address.[10] = 0xFFuy
                    && address.[11] = 0xFFuy

                if mapped then
                    let v4 = BinaryPrimitives.ReadUInt32BigEndian (address.AsSpan().Slice (12, 4))
                    Some (Ipv6Destination.V4Mapped (InternetEndpoint.ofParts v4 port))
                else
                    Some (Ipv6Destination.Native (address, port))
            else
                None

        {
            Family = family
            Destination = destination
        }

    /// The sixteen bytes of `sin6_addr` an IPv6 socket whose transport is
    /// IPv4 reports for the IPv4 `address`, in a socket whose connection has
    /// failed or not: its connect was refused, or a reset has reached it.
    ///
    /// The wildcard is `::` on both flavours -- an IPv6 socket bound only by a
    /// connect, or reverted by a refusal, reads back `[::]:port` -- and any
    /// other address is `::ffff:a.b.c.d`, except that Darwin reports a socket
    /// whose connection has failed at the IPv4-compatible `::a.b.c.d`, the
    /// `ffff` gone. That holds once a call has taken the error, and whichever
    /// way the reset came: the peer's close over bytes it had not read, a
    /// write after the peer's orderly close, or the peer's close under
    /// `SO_LINGER` {1, 0}. The peer's orderly close alone leaves the address
    /// v4-mapped. Measured (`docs/probes/dual-mode/dual-mode.c`, A, E, G and
    /// R).
    let presentedIpv6Address (platform : SimulatedUnixPlatform) (connectionFailed : bool) (address : uint32) : byte[] =
        let bytes = Array.zeroCreate<byte> 16

        if address <> InternetEndpoint.WildcardAddress then
            BinaryPrimitives.WriteUInt32BigEndian (System.Span<byte> (bytes, 12, 4), address)

            match flavour platform with
            | SimulatedUnixFlavour.Darwin when connectionFailed -> ()
            | SimulatedUnixFlavour.Darwin
            | SimulatedUnixFlavour.Linux ->
                bytes.[10] <- 0xFFuy
                bytes.[11] <- 0xFFuy

        bytes

    /// `struct sockaddr_in6` for the sixteen bytes `address` and `port`, as
    /// this platform's kernel copies one out: the family, the port, a zero
    /// `sin6_flowinfo`, the address and a zero `sin6_scope_id`, and on Darwin
    /// 28 in the `sa_len` byte. Measured on both.
    let encodeInternetV6Sockaddr (platform : SimulatedUnixPlatform) (address : byte[]) (port : uint16) : byte[] =
        if address.Length <> InternetV6Sockaddr.address.Width then
            failwith
                $"SimulatedUnixPlatform.encodeInternetV6Sockaddr: an IPv6 address is %d{InternetV6Sockaddr.address.Width} bytes, and the caller passed %d{address.Length} (this is a bug in the caller)."

        let realLength = internetV6SocketAddressSize
        let blob = Array.zeroCreate<byte> realLength

        BinaryPrimitives.WriteUInt16BigEndian (
            System.Span<byte> (blob, InternetV6Sockaddr.port.Offset, InternetV6Sockaddr.port.Width),
            port
        )

        address.CopyTo (blob, InternetV6Sockaddr.address.Offset)

        let field = sockaddrFamilyField platform
        let familyBytes = encodeSockaddrFamily platform (internetV6AddressFamily platform)
        familyBytes.CopyTo (blob, SockaddrFamilyField.offset field)

        match field with
        | SockaddrFamilyField.OneByteAtOffsetOne -> blob.[0] <- byte realLength
        | SockaddrFamilyField.TwoBytesAtOffsetZero -> ()

        blob
