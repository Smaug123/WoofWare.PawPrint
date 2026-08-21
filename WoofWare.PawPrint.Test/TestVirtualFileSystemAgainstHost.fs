namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Differential test: the same filesystem is built twice — once as a
/// `VirtualFileSystem` and once as a real directory tree — and the same paths
/// are resolved through both, comparing outcomes.
///
/// This is the only oracle here that is not a restatement of PawPrint's own
/// beliefs. Probed: a trailing separator cannot be desugared into a "."
/// component — `mkdir("d/")` succeeds where `mkdir("d/.")` does not.
///
/// Deliberately avoids `struct stat`, whose layout is platform-specific and
/// whose symbol is versioned on macOS. `access(2)` and `readlink(2)` between
/// them classify every outcome the walk can produce, and both take only a
/// `char*` and return an integer.
///
/// Touching the real filesystem is fine in a *test*; the prohibition is on the
/// product, which must never read the host.
[<TestFixture>]
module TestVirtualFileSystemAgainstHost =

    [<DllImport("libc", SetLastError = true)>]
    extern int private access(string path, int mode)

    [<DllImport("libc", SetLastError = true)>]
    extern nativeint private readlink(string path, byte[] buf, nativeint bufsiz)

    [<DllImport("libc", SetLastError = true)>]
    extern int private symlink(string target, string linkpath)

    /// `open(2)`. Its C prototype is `int open(const char *, int, ...)`, so
    /// `mode` is a *variadic* argument and a fixed-signature P/Invoke may pass
    /// it somewhere the callee does not look — on Apple arm64 variadic arguments
    /// go on the stack while fixed ones go in registers. `path` and `flags` are
    /// fixed parameters and so are always correct, which is why nothing below
    /// asserts on the *mode* a created file ends up with: only on whether the
    /// call created, opened, or failed, none of which `mode` can change.
    [<DllImport("libc", SetLastError = true)>]
    extern int private ``open``(string path, int flags, int mode)

    [<DllImport("libc", SetLastError = true)>]
    extern int private close(int fd)

    [<DllImport("libc", SetLastError = true)>]
    extern int private mkdir(string path, uint32 mode)

    [<DllImport("libc", SetLastError = true)>]
    extern int private ftruncate(int fd, int64 length)

    [<DllImport("libc", SetLastError = true)>]
    extern nativeint private write(int fd, byte[] buf, nativeint count)

    /// `chown(2)`, used here only to hand a file to a group the caller belongs
    /// to. That is a precondition for setting `S_ISGID` at all: a non-root
    /// `chmod` silently drops the bit when the file's group is not one of the
    /// caller's, and a file created under the temporary directory inherits *its*
    /// group under BSD semantics — `wheel` on macOS.
    [<DllImport("libc", SetLastError = true)>]
    extern int private chown(string path, uint32 owner, uint32 group)

    [<DllImport("libc")>]
    extern uint32 private getegid()

    [<DllImport("libc")>]
    extern uint32 private geteuid()

    [<DllImport("libc", SetLastError = true)>]
    extern nativeint private realpath(string path, nativeint resolved)

    [<DllImport("libc")>]
    extern void private free(nativeint ptr)

    [<Literal>]
    let private F_OK = 0

    /// Which simulated platform this test host actually is, so the model is
    /// asked to resolve as a kernel of the flavour it is being compared against.
    ///
    /// A function rather than a value so that the `failwith` cannot fire during
    /// module initialisation on a host where every test here `Assert.Ignore`s.
    /// Only the *flavour* of the result is ever consumed, so `macOsArm64` is the
    /// right answer on an Intel Mac too.
    let private hostPlatform () : SimulatedUnixPlatform =
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            SimulatedUnixPlatform.macOsArm64
        elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            SimulatedUnixPlatform.linuxX64
        else
            failwith
                "TestVirtualFileSystemAgainstHost: this host is neither macOS nor Linux, so there is no SimulatedUnixPlatform to compare it against. Every test in this fixture is supposed to Assert.Ignore before reaching here."

    let private limits () : PathLimits =
        SimulatedUnixPlatform.pathLimits (hostPlatform ())

    /// How a path resolution finished, in terms both the model and the kernel
    /// can express without needing `struct stat`.
    [<RequireQualifiedAccess>]
    type private Outcome =
        /// The path resolved to a symlink (only reachable without following the
        /// final component), whose target is these exact bytes.
        | Symlink of target : string
        /// The path resolved to something that is not a symlink.
        | NotASymlink
        /// The path did not resolve, with this errno.
        | Failed of errno : int

    let private errno () : int = Marshal.GetLastPInvokeError ()

    /// The physical path of `path`, with every symlink in it resolved away.
    ///
    /// On macOS `Path.GetTempPath()` returns "/var/folders/...", and "/var" is
    /// itself a symlink to "/private/var". Every absolute path built under the
    /// raw temporary directory therefore spends one symlink traversal before
    /// reaching anything this test created — which silently shifts the measured
    /// limit down by one.
    let private physicalPath (path : string) : string =
        let resolved = realpath (path, 0n)

        if resolved = 0n then
            failwith $"realpath(%s{path}) failed: errno %d{errno ()}"

        try
            Marshal.PtrToStringUTF8 resolved
        finally
            free resolved

    /// The raw errno this host uses for a given error, which for `ELOOP` is
    /// the whole reason `UnixError` refuses to pick one: the model speaks
    /// `UnixError`, the kernel speaks numbers, and the two only meet once a
    /// platform is named — which a *test* may do, because it knows which host
    /// it is running on.
    let private hostErrno (error : UnixError) : int =
        match (UnixError.numbering error).Raw with
        | RawErrnoPortability.Portable value -> value
        | RawErrnoPortability.PlatformDependent (linux, darwin) ->
            if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
                darwin
            else
                linux

    // ------------------------------------------------------------- the corpus

    /// The tree built in both worlds. Symlink targets are all *relative*: a
    /// rooted target would resolve against the real root on the host and
    /// against the model's root in the model, which is a divergence created by
    /// the test rather than found by it.
    let private directories =
        [
            "d"
            "d/sub"
            "ns"
            "ns/kid"
            "ns/kid/gk"
            "cls677"
            "cls677/kid"
            "cls500"
            "cls500/kid"
        ]

    /// Directories whose mode is *not* the default, and the mode each ends up
    /// with. Applied after every directory, file and symlink exists, because
    /// three of them cannot be written or searched once narrowed.
    ///
    /// `ns` is the plain unsearchable case, two levels deep so that a walk can
    /// fail on an intermediate as well as on a holding directory. The other two
    /// are what separate "the owner triple decides" from "any class decides":
    /// measured on both kernels against a directory owned by the calling uid,
    /// 0o677 is EACCES to its owner though group and other may search it, and
    /// 0o500 is searchable though nobody else may.
    ///
    /// Every mode here keeps the owner's *read* bit, and must. The creating-open
    /// comparison stops at `CreatingOpenRules.verdict`, which does not model the
    /// access-mode permission check the handler applies afterwards — so a
    /// directory this caller could not open `O_RDONLY` would make the real
    /// `open(2)` fail for a reason the model here never sees. 0o100 is the
    /// sharpest search-only mode and is exactly that case; it is pinned in
    /// `TestVirtualFileSystem` instead, where the comparison is against the walk
    /// alone.
    let private narrowedDirectories =
        [ "ns", 0o666 ; "cls677", 0o677 ; "cls500", 0o500 ]

    let private files = [ "f" ; "d/g" ; "ns/file" ]

    let private symlinks =
        [
            // The ordinary cases.
            "ld", "d"
            "lf", "f"
            // Dangling, in the two ways that differ: a missing final name, and
            // a missing intermediate directory.
            "dang", "nx"
            "deep", "nx1/nx2"
            // A target carrying its own trailing separator, which imposes a
            // directory demand the guest's path never mentioned.
            "lfslash", "f/"
            "ldslash", "d/"
            // A target that navigates.
            "up", "d/../f"
            // The two shapes that must terminate: a plain cycle, and the
            // self-extending link that defeats cycle detection.
            "cycleA", "cycleB"
            "cycleB", "cycleA"
            "selfext", "selfext/x"
            // A target no path parser may normalise, checked verbatim.
            "weird", "a//b/"
            // Into the unsearchable directory's interior. A component spliced in
            // from a symlink target is looked up like any other, so this must
            // earn the same EACCES that naming it directly does.
            "lns", "ns"
        ]

    /// The paths resolved through both worlds.
    ///
    /// No path may escape above the root: the model's root is its own parent
    /// while the host's temporary directory has a real one, so "/.." means
    /// different things by construction. "d/.." is fine and is included.
    let private probePaths =
        [
            "" // the empty path
            "/"
            "."
            "d"
            "d/"
            "d/."
            "d/.."
            "d//sub"
            "d/./sub"
            "d/sub"
            "d/sub/.."
            "d/sub/"
            "/d"
            "/d/sub"
            "f"
            "f/"
            "f/."
            "f/x"
            "d/g"
            "nx"
            "nx/"
            "nx/y"
            "ld"
            "ld/"
            "ld/sub"
            "ld/g"
            "lf"
            "lf/"
            "dang"
            "dang/"
            "deep"
            "lfslash"
            "ldslash"
            "ldslash/sub"
            "up"
            "cycleA"
            // A cyclic link with a trailing separator. Every lookup traverses it
            // and gives ELOOP, as does Darwin's `mkdir`; Linux's `mkdir` never
            // dereferences it and gives EEXIST, which is the row that separates
            // `TrailingSeparatorPolicy.Ignore` from `Demand`.
            "cycleA/"
            "selfext"
            // readlink(2) does not follow a *final* symlink, so a cycle only
            // shows up as ELOOP when it sits part-way along a path. Without
            // these two, the ELOOP arm of the comparison would never run.
            "cycleA/x"
            "selfext/x"
            "weird"
            // Through an unsearchable directory: the object itself resolves
            // (nothing looks inside it), while every path *through* it is
            // EACCES — whether the component exists, is missing, is over-long,
            // or is reached by splicing a symlink target.
            "ns"
            "ns/"
            "ns/kid"
            "ns/kid/gk"
            "ns/file"
            "ns/nx"
            "ns/."
            "ns/.."
            "lns/kid"
            // The class-asymmetric pair: an ordinary corpus cannot tell "the
            // owner triple decides" from "any execute bit decides".
            "cls677"
            "cls677/kid"
            "cls500"
            "cls500/kid"
        ]

    // ------------------------------------------------------------ the host side

    let private hostPath (root : string) (relative : string) : string =
        if relative = "" then
            // Prefixing would turn the one path every Unix rejects into a
            // perfectly good one.
            ""
        elif relative.StartsWith ("/", StringComparison.Ordinal) then
            root + relative
        else
            root + "/" + relative

    /// `UnixFileMode`'s members are numbered as the mode bits themselves, so the
    /// conversion each way is a cast rather than a table. Asserted by
    /// ``UnixFileMode is numbered as the raw mode bits`` below, so an upstream
    /// renumbering fails there rather than silently comparing the wrong bits.
    let private unixFileModeOf (raw : int) : UnixFileMode = enum<UnixFileMode> raw

    let private rawModeOf (mode : UnixFileMode) : int = int mode

    /// Gives `path` the mode, reporting whether it took. A refusal is not a
    /// failure of the thing under test: Nix's Linux build sandbox answers EPERM
    /// to any `chmod` setting S_ISUID or S_ISGID ("Prevent builders from
    /// creating setuid/setgid binaries", in `linux-derivation-builder.cc`), and
    /// a kernel may also drop a set-ID bit silently. Either way the row cannot
    /// be compared, so it is dropped and counted rather than believed.
    let private trySetMode (path : string) (mode : int) : bool =
        try
            File.SetUnixFileMode (path, unixFileModeOf mode)
            rawModeOf (File.GetUnixFileMode path) = mode
        with
        | :? UnauthorizedAccessException
        | :? IOException -> false

    /// The privilege the model must be asked at, so that it answers the question
    /// this host's kernel is actually answering. Root ignores every permission
    /// bit, so under a root test host the corpus's EACCES rows go vacuous rather
    /// than red — the same choice `compareTruncationModes` makes.
    let private hostPrivilege () : CallerPrivilege =
        if geteuid () = 0u then
            CallerPrivilege.Privileged
        else
            CallerPrivilege.Unprivileged

    let private buildHostTree (root : string) : unit =
        for directory in directories do
            Directory.CreateDirectory (Path.Combine (root, directory))
            |> ignore<DirectoryInfo>

        for file in files do
            File.WriteAllBytes (Path.Combine (root, file), Array.empty)

        for name, target in symlinks do
            // symlink(2) directly rather than File.CreateSymbolicLink, which
            // has opinions about targets that do not exist.
            if symlink (target, Path.Combine (root, name)) <> 0 then
                failwith $"could not create symlink %s{name} -> %s{target}: errno %d{errno ()}"

        for name, mode in narrowedDirectories do
            // Last, and it has to be: `ns/kid` and `ns/file` cannot be created
            // once `ns` is unsearchable. A refusal is fatal rather than skipped —
            // silently keeping 0o755 would make every row below agree for the
            // wrong reason.
            if not (trySetMode (Path.Combine (root, name)) mode) then
                failwith
                    $"could not set %s{name} to 0o%04o{mode}: this fixture cannot compare a permission rule it was not allowed to set up."

    /// Remove a corpus tree, widening the narrowed directories first.
    ///
    /// `Directory.Delete (root, true)` cannot unlink a child of a directory it
    /// may not search, and every caller wraps its teardown in `try … with _ ->
    /// ()`. Without this the failure is invisible and each row leaks a
    /// temporary tree — and the comparison helpers build one *per row*.
    let private removeHostTree (root : string) : unit =
        for name, _ in narrowedDirectories do
            trySetMode (Path.Combine (root, name)) 0o755 |> ignore<bool>

        try
            Directory.Delete (root, true)
        with _ ->
            ()

    let private hostOutcome (root : string) (relative : string) : Outcome =
        let path = hostPath root relative
        let buffer = Array.zeroCreate<byte> 4096
        let read = readlink (path, buffer, nativeint buffer.Length)

        if read >= 0n then
            Outcome.Symlink (Text.Encoding.UTF8.GetString (buffer, 0, int read))
        else

        match errno () with
        // readlink reports EINVAL when the path resolved to something that is
        // not a symlink, which is exactly "exists, not a link".
        | e when e = hostErrno UnixError.EINVAL ->
            if access (path, F_OK) = 0 then
                Outcome.NotASymlink
            else
                Outcome.Failed (errno ())
        | e -> Outcome.Failed e

    // ----------------------------------------------------------- the model side

    /// When the model's inodes are created. Irrelevant to every comparison in
    /// this file — the host side is compared on resolution outcomes, never on
    /// timestamps, which could not agree anyway — but a filesystem has to be
    /// built at *some* moment.
    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private buildModel () : VirtualFileSystem =
        let mutable vfs = VirtualFileSystem.empty buildTime

        let resolveDirectory (relative : string) : InodeNumber =
            match
                VirtualFileSystem.resolveExisting
                    (limits ())
                    // Privileged: this is the builder placing an inode, not a
                    // guest looking one up. `ns/kid/gk` could not be reached
                    // otherwise, since `ns` is narrowed below.
                    CallerPrivilege.Privileged
                    (VirtualFileSystem.root vfs)
                    SymlinkPolicy.Follow
                    (UnixPath.parseOrFail "test" relative)
                    vfs
            with
            | Ok inode -> inode
            | Error error -> failwith $"could not resolve %s{relative} while building the model: %O{error}"

        let split (relative : string) : InodeNumber * FileName =
            match relative.LastIndexOf '/' with
            | -1 -> VirtualFileSystem.root vfs, FileName.parseOrFail "test" relative
            | index ->
                resolveDirectory (relative.Substring (0, index)),
                FileName.parseOrFail "test" (relative.Substring (index + 1))

        let apply (result : Result<'a, UnixError>) (what : string) : 'a =
            match result with
            | Ok value -> value
            | Error error -> failwith $"could not create %s{what} in the model: %O{error}"

        let narrowed = Map.ofList narrowedDirectories

        for directory in directories do
            let parent, name = split directory

            // The narrowed ones are created *with* their mode rather than
            // chmod'ed afterwards, since this module has no chmod; the host tree
            // narrows last for the opposite reason. Both end at the same graph,
            // because a builder applies no permission rule of its own.
            let permissions =
                match Map.tryFind directory narrowed with
                | Some mode -> PermissionBits.parseOrFail "test" mode
                | None -> PermissionBits.defaultForDirectory

            let _, updated =
                apply (VirtualFileSystem.createDirectory parent name permissions buildTime vfs) directory

            vfs <- updated

        for file in files do
            let parent, name = split file

            let _, updated =
                apply
                    (VirtualFileSystem.createFile
                        parent
                        name
                        PermissionBits.defaultForRegularFile
                        buildTime
                        ImmutableArray<byte>.Empty
                        vfs)
                    file

            vfs <- updated

        for name, target in symlinks do
            let parent, leaf = split name

            let _, updated =
                apply
                    (VirtualFileSystem.createSymlink parent leaf buildTime (SymlinkTarget.parseOrFail "test" target) vfs)
                    name

            vfs <- updated

        VirtualFileSystem.assertInvariants "TestVirtualFileSystemAgainstHost" vfs

    let private modelOutcome (vfs : VirtualFileSystem) (relative : string) : Outcome =
        // readlink(2) does not follow a final symlink, which is exactly
        // NoFollowFinal — and a trailing separator overrides that on both sides.
        match
            VirtualFileSystem.resolveExisting
                (limits ())
                (hostPrivilege ())
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                (UnixPath.parseOrFail "test" relative)
                vfs
        with
        | Error error -> Outcome.Failed (hostErrno error)
        | Ok inode ->

        match VirtualFileSystem.tryGetContent inode vfs with
        | Some (InodeContent.Symlink target) -> Outcome.Symlink (SymlinkTarget.toString target)
        | Some _ -> Outcome.NotASymlink
        | None -> failwith $"the model resolved %s{relative} to inode %O{inode}, which it does not contain"

    /// Where this kernel's symlink limit actually sits: the longest chain that
    /// still resolves.
    let private hostSymlinkLimit (root : string) : int =
        let build (n : int) : bool =
            for i in 1..n do
                let link = Path.Combine (root, $"limit%d{n}_s%d{i}")

                let target =
                    if i = n then
                        $"limit%d{n}_target"
                    else
                        $"limit%d{n}_s%d{i + 1}"

                if symlink (target, link) <> 0 then
                    failwith $"could not build the probe chain: errno %d{errno ()}"

            File.WriteAllBytes (Path.Combine (root, $"limit%d{n}_target"), Array.empty)
            access (Path.Combine (root, $"limit%d{n}_s1"), F_OK) = 0

        // Search upwards from below every plausible limit rather than assuming
        // one; the point is to measure, not to restate the constant.
        let mutable n = 1

        while n < 100 && build n do
            n <- n + 1

        n - 1

    [<Test>]
    let ``pathLimits states this kernel's real symlink limit exactly`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // Pins `SimulatedUnixPlatform.pathLimits` against the kernel rather than
        // against a header or a recollection. macOS locally and Linux in CI, so
        // each flavour's entry is checked on the machine that can falsify it.
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-loop-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            let limit = hostSymlinkLimit root

            // Sanity: a limit of 0 or 99 would mean the probe measured nothing.
            limit |> shouldBeGreaterThan 7
            limit |> shouldBeSmallerThan 99

            let modelled =
                PathLimits.maxSymlinkTraversals (SimulatedUnixPlatform.pathLimits (hostPlatform ()))

            if limit <> modelled then
                failwith
                    $"This kernel resolves a chain of %d{limit} symlinks and refuses %d{limit + 1}, but SimulatedUnixPlatform.pathLimits says %O{hostPlatform ()} permits %d{modelled}. The model would disagree with a real kernel of the flavour it claims to be; %d{limit} is the measured answer."
        finally
            removeHostTree root

    /// Names whose treatment separates the three implementations anyone might
    /// write: counting bytes, counting characters, and counting UTF-16 units.
    ///
    /// `中` is 3 bytes and 1 unit; an emoji is 4 bytes, 1 character and 2 units.
    /// So 255 `中` distinguishes bytes from units, and 127 emoji + one ASCII
    /// distinguishes characters from units.
    let private nameProbes : (string * string) list =
        [
            "255 ASCII", String.replicate 255 "a"
            "256 ASCII", String.replicate 256 "a"
            "85 CJK (255 bytes)", String.replicate 85 "中"
            "86 CJK (258 bytes)", String.replicate 86 "中"
            "255 CJK (765 bytes, 255 units)", String.replicate 255 "中"
            "127 emoji + 1 ASCII (255 units)", String.replicate 127 "\U0001F600" + "a"
            "127 emoji + 2 ASCII (256 units)", String.replicate 127 "\U0001F600" + "aa"
            "255 e-acute in NFC (510 bytes, 255 units)", String.replicate 255 "é"
        ]

    [<Test>]
    let ``pathLimits agrees with this kernel about which names are too long`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // Compares the *predicate* rather than the number, which is the only
        // thing `PathLimits` exposes — deliberately, since a number without its
        // unit is meaningless. It is also the stronger comparison: it fails for
        // any name the model and the kernel disagree about, whatever arithmetic
        // produced the disagreement.
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-namemax-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root
        let limits = limits ()

        try
            for label, candidate in nameProbes do
                // ENAMETOOLONG rather than ENOENT is how the kernel says "too
                // long"; nothing here exists, so ENOENT means "would have been
                // allowed to exist".
                let hostPermits =
                    match access (Path.Combine (root, candidate), F_OK) with
                    | 0 -> failwith $"%s{label}: the probe name unexpectedly exists"
                    | _ ->

                    match errno () with
                    | e when e = hostErrno UnixError.ENOENT -> true
                    | e when e = hostErrno UnixError.ENAMETOOLONG -> false
                    | e -> failwith $"%s{label}: unexpected errno %d{e} from access(2)"

                let modelPermits =
                    PathLimits.nameWithinLimit limits (FileName.parseOrFail "name probe" candidate)

                if hostPermits <> modelPermits then
                    let verb (permits : bool) =
                        if permits then "permits" else "refuses"

                    failwith
                        $"%s{label} (%d{Text.Encoding.UTF8.GetByteCount candidate} UTF-8 bytes, %d{candidate.Length} UTF-16 units): this kernel %s{verb hostPermits} it, but PathLimits for %O{hostPlatform ()} %s{verb modelPermits} it."
        finally
            removeHostTree root

    /// The longest pathname argument this kernel accepts, in bytes. Every
    /// component is "." so that `NAME_MAX` cannot be what refuses it, and the
    /// path resolves whenever it is short enough.
    let private hostPathMax (root : string) : int =
        let accepts (n : int) : bool =
            let filler = String.replicate ((n / 2) + 1) "./"
            let candidate = filler.Substring (0, n)

            match access (Path.Combine (root, candidate), F_OK) with
            | 0 -> true
            | _ -> errno () <> hostErrno UnixError.ENAMETOOLONG

        // Bisect rather than scan: the two plausible answers are ~1024 apart on
        // one platform and ~4096 on the other.
        let mutable low = 1
        let mutable high = 65536

        while high - low > 1 do
            let mid = low + (high - low) / 2

            if accepts mid then low <- mid else high <- mid

        low

    [<Test>]
    let ``pathLimits states this kernel's real PATH_MAX exactly`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-pathmax-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            // The probe passes an *absolute* path, so the root's own bytes count
            // toward the limit; the usable length of a bare argument is that
            // plus what the prefix consumed.
            let prefix = root.Length + 1
            let usable = hostPathMax root + prefix

            let modelled = PathLimits.pathMaxBytes (limits ())

            // PATH_MAX counts the NUL, so the longest usable argument is one
            // less than it.
            if usable + 1 <> modelled then
                failwith
                    $"This kernel accepts a pathname argument of %d{usable} bytes and refuses %d{usable + 1}, so its PATH_MAX is %d{usable + 1}; SimulatedUnixPlatform.pathLimits says %O{hostPlatform ()} has %d{modelled}."
        finally
            removeHostTree root

    // ------------------------------------------------- symlink splice length

    /// An absolute path of exactly `bytes` bytes naming nothing, in components
    /// of 200 so that NAME_MAX cannot be what refuses it on either flavour.
    let private danglingTarget (bytes : int) : string =
        let component_ = "/" + String.replicate 200 "z"
        let repeated = String.replicate (bytes / component_.Length + 1) component_
        repeated.Substring (0, bytes)

    [<Test>]
    let ``the model splices symlink targets exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // Darwin re-checks the total length whenever it expands a symbolic link
        // and Linux does not, so this pins whichever of the two the host is —
        // macOS locally, Linux in CI.
        //
        // Outcomes are compared pointwise rather than boundaries, so the same
        // test is meaningful on a kernel that has no boundary at all: on Linux
        // every probe below must resolve, and a model that wrongly re-checked
        // would fail here rather than silently agreeing.
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-splice-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            let pathMax = PathLimits.pathMaxBytes (limits ())
            let mutable index = 0
            let mutable compared = 0

            // The suffix table from the probes, including the rows that
            // separate a raw-byte model from a canonical one: "//a" costs what
            // "/a" costs, but "/a//b" costs one byte more than "/a/b".
            let suffixes =
                [
                    ""
                    "/"
                    "//"
                    "/a"
                    "/a/"
                    "//a"
                    "///a"
                    "/a/b"
                    "/a//b"
                    "/a///b"
                    "/./a"
                    "/.."
                    "/a/../b"
                ]

            for suffix in suffixes do
                // Where the *model* would put the boundary. Used only to choose
                // probe points; every assertion below compares the host's
                // outcome against the model's, so a wrong PATH_MAX here would
                // move the probes rather than excuse a disagreement — and the
                // PATH_MAX oracle above is what pins that number.
                let predicted = pathMax - Text.Encoding.UTF8.GetByteCount suffix - 1

                for candidate in [ predicted - 1 ; predicted ; predicted + 1 ] do
                    // `symlink(2)` will not create a target of PATH_MAX bytes or
                    // more, so that cell is not reachable on a live kernel at
                    // all; the unit tests state the extrapolation instead.
                    if candidate >= 1 && candidate <= pathMax - 1 then
                        index <- index + 1
                        let linkName = $"L%d{index}"
                        let linkPath = Path.Combine (root, linkName)
                        let targetText = danglingTarget candidate

                        if symlink (targetText, linkPath) <> 0 then
                            failwith
                                $"could not create a probe symlink with a %d{candidate}-byte target: errno %d{errno ()}"

                        let hostOutcome =
                            match access (linkPath + suffix, F_OK) with
                            | 0 -> Ok ()
                            | _ -> Error (errno ())

                        let vfs =
                            VirtualFileSystem.createSymlink
                                (VirtualFileSystem.root (VirtualFileSystem.empty buildTime))
                                (FileName.parseOrFail "test" linkName)
                                buildTime
                                (SymlinkTarget.parseOrFail "test" targetText)
                                (VirtualFileSystem.empty buildTime)
                            |> function
                                | Ok (_, vfs) -> vfs
                                | Error error -> failwith $"building the model link: %O{error}"

                        let modelOutcome =
                            match
                                VirtualFileSystem.resolve
                                    (limits ())
                                    CallerPrivilege.Privileged
                                    (VirtualFileSystem.root vfs)
                                    SymlinkPolicy.Follow
                                    (UnixPath.parseOrFail "test" ("/" + linkName + suffix))
                                    vfs
                            with
                            | Ok _ -> Ok ()
                            | Error error -> Error (hostErrno error)

                        compared <- compared + 1

                        if hostOutcome <> modelOutcome then
                            let describe (outcome : Result<unit, int>) : string =
                                match outcome with
                                | Ok () -> "resolved"
                                | Error e when e = hostErrno UnixError.ENOENT -> "ENOENT"
                                | Error e when e = hostErrno UnixError.ENAMETOOLONG -> "ENAMETOOLONG"
                                | Error e when e = hostErrno UnixError.ELOOP -> "ELOOP"
                                | Error e -> $"errno %d{e}"

                            failwith
                                $"Resolving \"%s{linkName}%s{suffix}\" through a symlink whose target is %d{candidate} bytes: this kernel says %s{describe hostOutcome}, but the model says %s{describe modelOutcome}. %O{hostPlatform ()} either re-checks the spliced length or does not, and the kernel is the authority."

            // Guards against a silently empty sweep: if `symlink` or the probe
            // loop stopped producing candidates, every comparison above would
            // vacuously hold.
            compared |> shouldBeGreaterThan 30
        finally
            removeHostTree root

    // ------------------------------------------------------------------ the test

    [<Test>]
    let ``the model resolves every probe path exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-vfs-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            buildHostTree root
            let vfs = buildModel ()

            let mismatches =
                probePaths
                |> List.choose (fun relative ->
                    let expected = hostOutcome root relative
                    let actual = modelOutcome vfs relative

                    if expected = actual then
                        None
                    else
                        Some $"%s{relative}: kernel said %A{expected}, model said %A{actual}"
                )

            if not (List.isEmpty mismatches) then
                let rendered = String.Join (Environment.NewLine, mismatches)
                failwith $"The model disagrees with this kernel:%s{Environment.NewLine}%s{rendered}"

            // Guard against the corpus silently shrinking to nothing
            // interesting: every outcome shape must actually occur, or the
            // comparison above could pass by never exercising one.
            let observed = probePaths |> List.map (hostOutcome root) |> List.distinct

            observed
            |> List.exists (fun o ->
                match o with
                | Outcome.Symlink _ -> true
                | _ -> false
            )
            |> shouldEqual true

            observed |> List.contains Outcome.NotASymlink |> shouldEqual true

            observed
            |> List.contains (Outcome.Failed (hostErrno UnixError.ENOENT))
            |> shouldEqual true

            observed
            |> List.contains (Outcome.Failed (hostErrno UnixError.ENOTDIR))
            |> shouldEqual true

            observed
            |> List.contains (Outcome.Failed (hostErrno UnixError.ELOOP))
            |> shouldEqual true

            // The corpus's unsearchable directories must actually be
            // unsearchable. Without this a `chmod` that silently did nothing
            // would leave every permission row agreeing for the wrong reason —
            // and `buildHostTree` would have to have failed loudly for that to
            // happen, so this is the second line of defence rather than the
            // first. Root searches anything, so it cannot make the observation.
            if geteuid () <> 0u then
                observed
                |> List.contains (Outcome.Failed (hostErrno UnixError.EACCES))
                |> shouldEqual true
        finally
            removeHostTree root

    // --------------------------------------------------- creating opens

    /// What a creating `open(2)` did, in terms both worlds can express.
    [<RequireQualifiedAccess>]
    type private CreatingOutcome =
        /// The call succeeded and bound a name that was not there before.
        | Created
        /// The call succeeded on something that already existed.
        | Opened
        /// The call failed with this errno.
        | Failed of errno : int

    /// This kernel's own `O_CREAT` and `O_EXCL`, which — unlike the PAL values
    /// the interpreter consumes — are different numbers on the two platforms.
    let private hostOpenFlags () : int * int =
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            0x0200, 0x0800
        else
            0o100, 0o200

    let private hostCreatingOutcome (root : string) (relative : string) (exclusive : bool) : CreatingOutcome =
        let path = hostPath root relative
        let oCreat, oExcl = hostOpenFlags ()
        let flags = oCreat ||| (if exclusive then oExcl else 0)

        // Asked *before* the call, so a success can be classified without a
        // second syscall that would see the file this one may just have made.
        // `access` follows symlinks, which is the right question here:
        // `open(dang, O_CREAT)` creates dang's target, and it is that target
        // which did not exist.
        let existedBefore = access (path, F_OK) = 0

        let fd = ``open`` (path, flags, 0o666)

        if fd < 0 then
            CreatingOutcome.Failed (errno ())
        else

        close fd |> ignore<int>

        if existedBefore then
            CreatingOutcome.Opened
        else
            CreatingOutcome.Created

    let private modelCreatingOutcome
        (vfs : VirtualFileSystem)
        (relative : string)
        (exclusive : bool)
        : CreatingOutcome
        =
        let rules = SimulatedUnixPlatform.creatingOpenRules (hostPlatform ())

        // Exactly the policies `SystemNative_Open` selects for a creating open;
        // see the handler for why `O_EXCL` implies `NoFollowFinal`.
        let policy =
            if exclusive then
                SymlinkPolicy.NoFollowFinal
            else
                SymlinkPolicy.Follow

        match
            VirtualFileSystem.resolveFull
                (limits ())
                (hostPrivilege ())
                (VirtualFileSystem.root vfs)
                policy
                rules.TrailingSeparator
                (UnixPath.parseOrFail "test" relative)
                vfs
        with
        | Error error -> CreatingOutcome.Failed (hostErrno error)
        | Ok resolution ->

        // At the host's own privilege: the corpus *does* contain directories
        // whose owner bits are clear, so this arm is reachable and the two
        // worlds would disagree under a root host if it were hardcoded.
        match CreatingOpenRules.verdict rules (hostPrivilege ()) true exclusive resolution vfs with
        | CreatingOpenVerdict.Refuse error -> CreatingOutcome.Failed (hostErrno error)
        | CreatingOpenVerdict.Create _ -> CreatingOutcome.Created
        | CreatingOpenVerdict.OpenExisting _ -> CreatingOutcome.Opened

    /// `probePaths`, less the one path this comparison structurally cannot make.
    ///
    /// The model resolves "/" to a `FinalNavigation.Root` — a path that consumed
    /// *no component at all* — and Darwin answers EEXIST for exactly that shape
    /// even without `O_EXCL`. The host side cannot reproduce it: every path here
    /// is prefixed with the temporary root, so "/" arrives at the kernel as a
    /// directory reached by name, and is opened. That is the same reason this
    /// fixture already forbids paths escaping above its root. The Root rule is
    /// pinned against its measurement in `TestCreatingOpenRules` instead.
    let private creatingProbePaths = probePaths |> List.filter (fun path -> path <> "/")

    /// Run one creating open through both worlds, each built fresh — a creating
    /// open mutates, so no two rows may share a tree.
    let private compareCreatingOpen (relative : string) (exclusive : bool) : CreatingOutcome * CreatingOutcome =
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-create-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            buildHostTree root
            let vfs = buildModel ()
            hostCreatingOutcome root relative exclusive, modelCreatingOutcome vfs relative exclusive
        finally
            removeHostTree root

    [<Test>]
    let ``a creating open decides exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // The rows this pins are the ones no `sourcesPure` guest can carry. The
        // two kernels genuinely disagree about a creating open on a directory,
        // and about one whose final component carries a trailing separator, so
        // the model is instantiated at *this* host's flavour and macOS locally
        // and Linux in CI each falsify their own column. It also carries every
        // EEXIST row, which a managed guest cannot reach at all: building the
        // exception for EEXIST needs SystemNative_ConvertErrorPalToPlatform and
        // SystemNative_StrErrorR, neither of which exists.
        let mismatches =
            [
                for exclusive in [ false ; true ] do
                    for relative in creatingProbePaths do
                        let expected, actual = compareCreatingOpen relative exclusive

                        if expected <> actual then
                            let flags = if exclusive then "O_CREAT|O_EXCL" else "O_CREAT"

                            yield $"%s{flags} %s{relative}: kernel said %A{expected}, model said %A{actual}"
            ]

        if not (List.isEmpty mismatches) then
            let rendered = String.Join (Environment.NewLine, mismatches)

            failwith $"The model disagrees with this kernel about creating opens:%s{Environment.NewLine}%s{rendered}"

    [<Test>]
    let ``the creating-open corpus reaches every verdict this kernel can give`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // Without this the comparison above could pass by never creating
        // anything, or by never provoking a refusal. Stated against the
        // *kernel's* answers rather than the model's, so it cannot be satisfied
        // by the model agreeing with itself.
        let observed =
            [
                for exclusive in [ false ; true ] do
                    for relative in creatingProbePaths do
                        yield fst (compareCreatingOpen relative exclusive)
            ]
            |> List.distinct

        observed |> List.contains CreatingOutcome.Created |> shouldEqual true
        observed |> List.contains CreatingOutcome.Opened |> shouldEqual true

        // EEXIST is the whole point of `O_EXCL`, and ELOOP proves a creating
        // open still traverses links on the way in.
        observed
        |> List.contains (CreatingOutcome.Failed (hostErrno UnixError.EEXIST))
        |> shouldEqual true

        observed
        |> List.contains (CreatingOutcome.Failed (hostErrno UnixError.ELOOP))
        |> shouldEqual true

        // The divergent verdict, asserted per flavour so that the corpus cannot
        // quietly lose the rows which separate the two kernels. Linux refuses a
        // creating open on a directory or on a trailing separator; Darwin opens
        // the directory instead and reports ENOTDIR only for the paths that run
        // through a regular file.
        if RuntimeInformation.IsOSPlatform OSPlatform.Linux then
            observed
            |> List.contains (CreatingOutcome.Failed (hostErrno UnixError.EISDIR))
            |> shouldEqual true
        else
            observed
            |> List.contains (CreatingOutcome.Failed (hostErrno UnixError.ENOTDIR))
            |> shouldEqual true

    // ------------------------------------------------------- truncation's mode

    [<Test>]
    let ``UnixFileMode is numbered as the raw mode bits`` () : unit =
        int UnixFileMode.SetUser |> shouldEqual 0o4000
        int UnixFileMode.SetGroup |> shouldEqual 0o2000
        int UnixFileMode.StickyBit |> shouldEqual 0o1000
        int UnixFileMode.UserRead |> shouldEqual 0o0400
        int UnixFileMode.UserWrite |> shouldEqual 0o0200
        int UnixFileMode.UserExecute |> shouldEqual 0o0100
        int UnixFileMode.GroupRead |> shouldEqual 0o0040
        int UnixFileMode.GroupWrite |> shouldEqual 0o0020
        int UnixFileMode.GroupExecute |> shouldEqual 0o0010
        int UnixFileMode.OtherRead |> shouldEqual 0o0004
        int UnixFileMode.OtherWrite |> shouldEqual 0o0002
        int UnixFileMode.OtherExecute |> shouldEqual 0o0001

    /// `O_WRONLY`, which is 1 on every Unix — unlike `O_CREAT`, this one needs no
    /// per-platform table.
    [<Literal>]
    let private hostOWrOnly = 0x0001

    /// The set-ID rows: the ones whose answer differs between the flavours.
    /// `0o4644` and `0o2644` are what tell the real rule from "strip both bits
    /// whenever either is set", and `0o6644` — both bits, no group-execute — is
    /// the row on which the three plausible rules give three different answers.
    ///
    /// Every mode here carries the user-write bit, because the comparisons below
    /// reopen the file `O_WRONLY`; `0o2444` would make the *open* the thing that
    /// failed. Both operations were measured for all of these, on macOS 26.6 and
    /// on Linux 6.18.5, so the list is honest for the write comparison and the
    /// truncation comparison alike.
    let private setIdModeProbes : int list =
        [
            0o4755
            0o4644
            0o2755
            0o2644
            0o6755
            0o2600
            0o2640
            0o6644
            0o3755
        ]

    /// The rows both flavours agree on, which is every mode carrying no set-ID
    /// bit. Each needs the user-write bit, or the `O_WRONLY` open below would be
    /// the thing that failed.
    let private ordinaryModeProbes : int list =
        [ 0o0644 ; 0o0755 ; 0o0600 ; 0o0666 ; 0o1755 ]

    /// Truncates a fresh four-byte file at each mode and compares what the
    /// kernel left behind against `PermissionBits.afterTruncation`. Returns how
    /// many rows were compared, which is not necessarily the length of `modes`.
    let private compareTruncationModes (root : string) (modes : int list) : int =
        let rule = SimulatedUnixPlatform.setIdBitsOnTruncation (hostPlatform ())

        let privilege =
            if geteuid () = 0u then
                CallerPrivilege.Privileged
            else
                CallerPrivilege.Unprivileged

        let mutable compared = 0

        for mode in modes do
            let path = Path.Combine (root, $"m%04o{mode}")
            File.WriteAllBytes (path, [| 1uy ; 2uy ; 3uy ; 4uy |])

            // Hand it to a group we are in, or `chmod` will drop S_ISGID.
            if chown (path, 0xFFFFFFFFu, getegid ()) <> 0 then
                failwith $"chown(%s{path}) failed: errno %d{errno ()}"

            if trySetMode path mode then
                let fd = ``open`` (path, hostOWrOnly, 0)

                if fd < 0 then
                    failwith $"open(%s{path}, O_WRONLY) failed: errno %d{errno ()}"

                let result = ftruncate (fd, 0L)
                let truncateErrno = errno ()
                close fd |> ignore<int>

                if result <> 0 then
                    failwith $"ftruncate(%s{path}, 0) failed: errno %d{truncateErrno}"

                let kernelSaid = rawModeOf (File.GetUnixFileMode path)

                let modelSaid =
                    PermissionBits.parseOrFail "test" mode
                    |> PermissionBits.afterTruncation rule privilege
                    |> PermissionBits.toInt

                if kernelSaid <> modelSaid then
                    failwith
                        $"truncating a 0o%04o{mode} file: this kernel left it 0o%04o{kernelSaid} but PermissionBits.afterTruncation says 0o%04o{modelSaid} under %O{rule} as %O{privilege}."

                compared <- compared + 1

        compared

    /// Writes one byte over the front of a fresh four-byte file at each mode and
    /// compares what the kernel left behind against
    /// `PermissionBits.afterContentChangingWrite`. Returns how many rows were
    /// compared, which is not necessarily the length of `modes`.
    ///
    /// Deliberately a *content-changing* write of one byte rather than a write of
    /// none: a zero-length write is not a write at all, and strips nothing.
    let private compareWriteModes (root : string) (modes : int list) : int =
        let rule = SimulatedUnixPlatform.setGroupIdOnWrite (hostPlatform ())

        let privilege =
            if geteuid () = 0u then
                CallerPrivilege.Privileged
            else
                CallerPrivilege.Unprivileged

        let mutable compared = 0

        for mode in modes do
            let path = Path.Combine (root, $"w%04o{mode}")
            File.WriteAllBytes (path, [| 1uy ; 2uy ; 3uy ; 4uy |])

            // Hand it to a group we are in, or `chmod` will drop S_ISGID.
            if chown (path, 0xFFFFFFFFu, getegid ()) <> 0 then
                failwith $"chown(%s{path}) failed: errno %d{errno ()}"

            if trySetMode path mode then
                let fd = ``open`` (path, hostOWrOnly, 0)

                if fd < 0 then
                    failwith $"open(%s{path}, O_WRONLY) failed: errno %d{errno ()}"

                let written = write (fd, [| 0x78uy |], 1n)
                let writeErrno = errno ()
                close fd |> ignore<int>

                if written <> 1n then
                    failwith $"write(%s{path}, 1 byte) wrote %d{written}: errno %d{writeErrno}"

                let kernelSaid = rawModeOf (File.GetUnixFileMode path)

                let modelSaid =
                    PermissionBits.parseOrFail "test" mode
                    |> PermissionBits.afterContentChangingWrite rule privilege
                    |> PermissionBits.toInt

                if kernelSaid <> modelSaid then
                    failwith
                        $"writing to a 0o%04o{mode} file: this kernel left it 0o%04o{kernelSaid} but PermissionBits.afterContentChangingWrite says 0o%04o{modelSaid} under %O{rule} as %O{privilege}."

                compared <- compared + 1

        compared

    /// Makes a directory of its own on the real filesystem, hands the path to
    /// `body`, and deletes it afterwards. Shared by the write and truncation
    /// comparisons, which want the same thing.
    let private withModeProbeRoot (body : string -> unit) : unit =
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-modeprobe-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            body root
        finally
            Directory.Delete (root, true)

    [<Test>]
    let ``truncation leaves a file's ordinary permission bits alone`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        withModeProbeRoot (fun root ->
            // Every row must land: nothing refuses a `chmod` that sets no set-ID
            // bit, so a dropped one here is a surprise to fail on rather than a
            // host that cannot answer.
            compareTruncationModes root ordinaryModeProbes
            |> shouldEqual (List.length ordinaryModeProbes)
        )

    [<Test>]
    let ``truncation moves a file's set-ID bits exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        if geteuid () = 0u then
            // Root preserves every bit on both flavours, so the whole table
            // collapses to the identity and could not falsify anything. The
            // suite does not choose the uid it runs as, so this is a skip rather
            // than a failure.
            Assert.Ignore "Running as root, where neither flavour strips and the comparison is vacuous."

        withModeProbeRoot (fun root ->
            match compareTruncationModes root setIdModeProbes with
            | 0 ->
                // `nix build` lands here and the `dotnet test` CI job does not,
                // so the Linux half of the table is still measured on every push
                // — just by the other job.
                Assert.Ignore
                    "This host refuses to set S_ISUID and S_ISGID at all, so no row here can tell the flavours apart."
            | compared ->
                // All-or-nothing: a host that takes some set-ID bits and drops
                // others is one we do not understand, and this names the row.
                compared |> shouldEqual (List.length setIdModeProbes)
        )

    [<Test>]
    let ``a content-changing write leaves a file's ordinary permission bits alone`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        withModeProbeRoot (fun root ->
            // Every row must land: nothing refuses a `chmod` that sets no set-ID
            // bit, so a dropped one here is a surprise to fail on rather than a
            // host that cannot answer.
            compareWriteModes root ordinaryModeProbes
            |> shouldEqual (List.length ordinaryModeProbes)
        )

    [<Test>]
    let ``a content-changing write moves a file's set-ID bits exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        if geteuid () = 0u then
            // Root strips nothing on either flavour, so the whole table collapses
            // to the identity and could not falsify anything. The suite does not
            // choose the uid it runs as, so this is a skip rather than a failure.
            Assert.Ignore "Running as root, where neither flavour strips and the comparison is vacuous."

        withModeProbeRoot (fun root ->
            match compareWriteModes root setIdModeProbes with
            | 0 ->
                // `nix build` lands here and the `dotnet test` CI job does not,
                // so the Linux half of the table is still measured on every push
                // — just by the other job.
                Assert.Ignore
                    "This host refuses to set S_ISUID and S_ISGID at all, so no row here can tell the flavours apart."
            | compared ->
                // All-or-nothing: a host that takes some set-ID bits and drops
                // others is one we do not understand, and this names the row.
                compared |> shouldEqual (List.length setIdModeProbes)
        )

    // ------------------------------------------------ the S_IFMT band's values

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails. Same shape
    /// as `TestUnixError.requireRuntimeSrc`, which is private to its own module.
    // --------------------------------------------------------------- mkdir

    /// What a `mkdir(2)` did, in terms both worlds can express.
    [<RequireQualifiedAccess>]
    type private MkDirOutcome =
        /// The call bound a new directory. *Which* name it bound is not recorded
        /// here — on Darwin `mkdir("dang/")` binds the link's target rather than
        /// anything the path names — because the host side cannot say. That
        /// identity is pinned instead by `MkDirRules.verdict`'s unit rows, which
        /// see the bound name directly, and by the Darwin wiring guest, which
        /// looks for the target afterwards.
        | Created
        /// The call failed with this errno.
        | Failed of errno : int

    let private hostMkDirOutcome (root : string) (relative : string) : MkDirOutcome =
        if mkdir (hostPath root relative, 0o777u) = 0 then
            MkDirOutcome.Created
        else
            MkDirOutcome.Failed (errno ())

    let private modelMkDirOutcome (vfs : VirtualFileSystem) (relative : string) : MkDirOutcome =
        let rules = SimulatedUnixPlatform.mkDirRules (hostPlatform ())

        match
            VirtualFileSystem.resolveFull
                (limits ())
                (hostPrivilege ())
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.NoFollowFinal
                rules.TrailingSeparator
                (UnixPath.parseOrFail "test" relative)
                vfs
        with
        | Error error -> MkDirOutcome.Failed (hostErrno error)
        | Ok resolution ->

        // At the host's own privilege, for the same reason as the creating-open
        // comparison: the corpus contains directories this caller may not write
        // or search, so the answer depends on who is asking.
        match MkDirRules.verdict (hostPrivilege ()) resolution vfs with
        | MkDirVerdict.Refuse error -> MkDirOutcome.Failed (hostErrno error)
        | MkDirVerdict.Create _ -> MkDirOutcome.Created

    /// `probePaths`, less the one path this comparison structurally cannot make.
    ///
    /// "/" is the host's real root here, since every other path is prefixed with
    /// the temporary directory; `mkdir("/")` would be asking the machine's root
    /// rather than the corpus's. The model's answer for a path that consumed no
    /// component at all is pinned in `TestVirtualFileSystem` instead, against
    /// the measurement (EEXIST on both).
    let private mkDirProbePaths = probePaths |> List.filter (fun path -> path <> "/")

    /// Run one `mkdir` through both worlds, each built fresh — `mkdir` mutates,
    /// so no two rows may share a tree.
    let private compareMkDir (relative : string) : MkDirOutcome * MkDirOutcome =
        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-mkdir-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            buildHostTree root
            let vfs = buildModel ()
            hostMkDirOutcome root relative, modelMkDirOutcome vfs relative
        finally
            removeHostTree root

    [<Test>]
    let ``mkdir decides exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // The rows that matter most here are the ones the two kernels answer
        // differently, and they are all trailing-separator rows: "f/", "lf/",
        // "dang/" and "cycleA/" are EEXIST on Linux — whose creating lookup
        // never dereferences the last component — and ENOTDIR, ENOTDIR, a
        // *creation*, and ELOOP on Darwin. macOS locally and Linux in CI each
        // falsify their own column.
        let mismatches =
            [
                for relative in mkDirProbePaths do
                    let expected, actual = compareMkDir relative

                    if expected <> actual then
                        yield $"mkdir %s{relative}: kernel said %A{expected}, model said %A{actual}"
            ]

        if not (List.isEmpty mismatches) then
            let rendered = String.Join (Environment.NewLine, mismatches)

            failwith $"The model disagrees with this kernel about mkdir:%s{Environment.NewLine}%s{rendered}"

    [<Test>]
    let ``the mkdir corpus reaches every verdict this kernel can give`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        // Without this the comparison above could pass by never creating
        // anything, or by never provoking a refusal. Stated against the
        // *kernel's* answers rather than the model's, so it cannot be satisfied
        // by the model agreeing with itself.
        let observed =
            mkDirProbePaths
            |> List.map (fun relative -> fst (compareMkDir relative))
            |> List.distinct

        observed |> List.contains MkDirOutcome.Created |> shouldEqual true

        observed
        |> List.contains (MkDirOutcome.Failed (hostErrno UnixError.EEXIST))
        |> shouldEqual true

        observed
        |> List.contains (MkDirOutcome.Failed (hostErrno UnixError.ENOENT))
        |> shouldEqual true

        // The divergent verdicts, asserted per flavour so that the corpus cannot
        // quietly lose the rows which separate the two kernels. Darwin resolves
        // a trailing separator as a lookup would, so it alone reaches ENOTDIR
        // (through "f/" and "lf/") and ELOOP (through "cycleA/"); Linux answers
        // EEXIST for all three, which the shared assertion above already covers.
        if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
            observed
            |> List.contains (MkDirOutcome.Failed (hostErrno UnixError.ENOTDIR))
            |> shouldEqual true

            observed
            |> List.contains (MkDirOutcome.Failed (hostErrno UnixError.ELOOP))
            |> shouldEqual true
        else
            // Linux's own ENOTDIR row: a path *through* a regular file, where the
            // walk fails before the last component and the creating lookup never
            // gets a say.
            observed
            |> List.contains (MkDirOutcome.Failed (hostErrno UnixError.ENOTDIR))
            |> shouldEqual true

    /// The umask this test host is actually running under, measured rather than
    /// asked for: `umask(2)` is process-global, and reading it means setting it,
    /// which would race every other fixture. A directory created with mode 0o777
    /// comes back with exactly the bits the umask left, so the complement of
    /// what a fresh one reports *is* the umask.
    let private observedUmask (root : string) : PermissionBits =
        let path = Path.Combine (root, "umask-probe")

        if mkdir (path, 0o777u) <> 0 then
            failwith $"mkdir(%s{path}, 0o777) failed: errno %d{errno ()}"

        0o777 &&& ~~~(rawModeOf (File.GetUnixFileMode path))
        |> PermissionBits.parseOrFail "test"

    /// Creates a directory at each mode inside `parent` and compares what the
    /// kernel left behind against `MkDirRules.createdPermissions`. Returns how
    /// many rows were compared.
    let private compareMkDirModes (root : string) (parent : string) (modes : int list) : int =
        let rules = SimulatedUnixPlatform.mkDirRules (hostPlatform ())
        let umask = observedUmask root

        let parentBits =
            rawModeOf (File.GetUnixFileMode parent) |> PermissionBits.parseOrFail "test"

        let mutable compared = 0

        for mode in modes do
            let path = Path.Combine (parent, $"m%04o{mode}")

            if mkdir (path, uint32 mode) <> 0 then
                failwith $"mkdir(%s{path}, 0o%04o{mode}) failed: errno %d{errno ()}"

            let kernelSaid = rawModeOf (File.GetUnixFileMode path)

            let modelSaid =
                MkDirRules.createdPermissions rules parentBits umask mode
                |> PermissionBits.toInt

            if kernelSaid <> modelSaid then
                failwith
                    $"mkdir at mode 0o%05o{mode} inside a 0o%04o{PermissionBits.toInt parentBits} directory: this kernel made it 0o%04o{kernelSaid} but MkDirRules.createdPermissions says 0o%04o{modelSaid} under umask 0o%04o{PermissionBits.toInt umask}."

            compared <- compared + 1

        compared

    /// Every mode worth asking about: the ordinary one CoreLib passes, the three
    /// upper bits one at a time, all of them at once, one above the permission
    /// word, and zero.
    let private mkDirModeProbes : int list =
        [ 0o777 ; 0o7777 ; 0o1777 ; 0o2777 ; 0o4777 ; 0o10777 ; 0o666 ; 0o000 ]

    [<Test>]
    let ``a new directory keeps exactly the mode bits this kernel keeps`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-mkdir-mode-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            // The whole point of the row set: `mkdir`'s mask is not `open`'s.
            // Linux keeps the sticky bit and drops both set-ID bits, so 0o7777
            // lands as 0o1755 here where a creating open lands 0o7755.
            compareMkDirModes root root mkDirModeProbes
            |> shouldEqual (List.length mkDirModeProbes)
        finally
            removeHostTree root

    [<Test>]
    let ``a new directory inherits set-group-ID exactly as this kernel does`` () : unit =
        if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
            Assert.Ignore "This oracle compares against a Unix kernel."

        let unique = Guid.NewGuid().ToString "N"
        let root = Path.Combine (Path.GetTempPath (), $"pawprint-mkdir-sgid-%s{unique}")
        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let root = physicalPath root

        try
            let parent = Path.Combine (root, "sgid")

            if mkdir (parent, 0o777u) <> 0 then
                failwith $"mkdir(%s{parent}) failed: errno %d{errno ()}"

            // Hand it to a group we are in, or `chmod` will drop S_ISGID —
            // silently, which is exactly how a probe of this shape reads as
            // "unsupported on this platform" when it is nothing of the kind.
            if chown (parent, 0xFFFFFFFFu, getegid ()) <> 0 then
                failwith $"chown(%s{parent}) failed: errno %d{errno ()}"

            if not (trySetMode parent 0o2777) then
                // A sandbox may refuse the chmod outright. Skipping is honest:
                // the alternative is measuring a parent that never carried the
                // bit and concluding the platform does not inherit it.
                Assert.Ignore "This host would not let the test set S_ISGID on a directory."

            compareMkDirModes root parent mkDirModeProbes
            |> shouldEqual (List.length mkDirModeProbes)
        finally
            removeHostTree root

    let private requireRuntimeSrc () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir -> dir

    /// `internal const int S_IFDIR = 0x4000;` and friends.
    let private fileTypeEntry : Text.RegularExpressions.Regex =
        Text.RegularExpressions.Regex (@"internal const int (?<name>S_IF[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+);")

    [<Test>]
    let ``the derived S_IFMT band agrees with the pinned Interop.Stat.cs`` () : unit =
        // `fileTypeBits` is where PawPrint decides what a guest's
        // `st_mode & S_IFMT` says. Checking it against a second copy of the same
        // literals would prove nothing, so the oracle is upstream's own
        // declaration — the very numbers the guest's CoreLib will compare
        // against.
        let path =
            Path.Combine (
                requireRuntimeSrc (),
                "src",
                "libraries",
                "Common",
                "src",
                "Interop",
                "Unix",
                "System.Native",
                "Interop.Stat.cs"
            )

        if not (File.Exists path) then
            failwith
                $"expected the pinned FileStatus declaration at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common/src/Interop/Unix/System.Native, VirtualFileSystem.fileTypeBits has lost its oracle."

        let pinned =
            fileTypeEntry.Matches (File.ReadAllText path)
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        // Guard against the regex silently matching nothing, which would make
        // every assertion below vacuous: upstream declares eight file types.
        pinned |> Map.count |> shouldEqual 8

        let ofName (name : string) : int =
            match Map.tryFind name pinned with
            | Some value -> value
            | None -> failwith $"the pinned Interop.Stat.cs no longer declares %s{name}"

        VirtualFileSystem.fileTypeBits (
            InodeContent.RegularFile (ImmutableArray<byte>.Empty, PermissionBits.defaultForRegularFile)
        )
        |> shouldEqual (ofName "S_IFREG")

        VirtualFileSystem.fileTypeBits (
            InodeContent.Directory
                {
                    Entries = Map.empty
                    Parent = InodeNumber 1L
                    Permissions = PermissionBits.defaultForDirectory
                }
        )
        |> shouldEqual (ofName "S_IFDIR")

        VirtualFileSystem.fileTypeBits (InodeContent.Symlink (SymlinkTarget.parseOrFail "test" "x"))
        |> shouldEqual (ofName "S_IFLNK")

        // ...and each of them really is inside the band, so that a value that
        // happened to match a typo'd constant still could not be a plausible
        // file type.
        let mask = ofName "S_IFMT"

        for content in
            [
                InodeContent.RegularFile (ImmutableArray<byte>.Empty, PermissionBits.defaultForRegularFile)
                InodeContent.Symlink (SymlinkTarget.parseOrFail "test" "x")
            ] do
            let bits = VirtualFileSystem.fileTypeBits content
            bits &&& mask |> shouldEqual bits
