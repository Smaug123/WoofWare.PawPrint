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
/// beliefs, and it earns its keep: it was written after a probe of this kernel
/// falsified the assumption that a trailing separator can be desugared into a
/// "." component (`mkdir("d/")` succeeds where `mkdir("d/.")` does not), which
/// an earlier draft of the resolver relied on.
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
    /// Load-bearing rather than tidy-minded: on macOS `Path.GetTempPath()`
    /// returns "/var/folders/...", and "/var" is itself a symlink to
    /// "/private/var". Every absolute path built under the raw temporary
    /// directory therefore spends one symlink traversal before reaching
    /// anything this test created — which silently shifts the measured limit
    /// down by one, and made an earlier version of the boundary test below
    /// conclude this kernel allows 31 traversals when it allows 32.
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
    let private directories = [ "d" ; "d/sub" ]

    let private files = [ "f" ; "d/g" ]

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
            "selfext"
            // readlink(2) does not follow a *final* symlink, so a cycle only
            // shows up as ELOOP when it sits part-way along a path. Without
            // these two, the ELOOP arm of the comparison would never run.
            "cycleA/x"
            "selfext/x"
            "weird"
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

        for directory in directories do
            let parent, name = split directory

            let _, updated =
                apply
                    (VirtualFileSystem.createDirectory parent name PermissionBits.defaultForDirectory buildTime vfs)
                    directory

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
        // against a header or a recollection. A review of this code claimed
        // macOS fails at 32 rather than 33; measuring settled it, and this test
        // keeps the answer from drifting. macOS locally and Linux in CI, so each
        // flavour's entry is checked on the machine that can actually falsify
        // it — and neither rests on my say-so.
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
            try
                Directory.Delete (root, true)
            with _ ->
                ()

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
            try
                Directory.Delete (root, true)
            with _ ->
                ()

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
            try
                Directory.Delete (root, true)
            with _ ->
                ()

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
        finally
            try
                Directory.Delete (root, true)
            with _ ->
                ()

    // ------------------------------------------------ the S_IFMT band's values

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails. Same shape
    /// as `TestUnixError.requireRuntimeSrc`, which is private to its own module.
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
