namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `FileStatusPal` transcribes the one bit of `st_flags` the shim keeps, so
/// nothing in the type system keeps its numbers right. Its oracles are the
/// pinned `pal_io.h` and, on a Darwin host, the host's own `SystemNative_LStat`
/// over files whose flags this test sets. The `st_mode` the kernel reports goes
/// into `FileStatus.Mode` untranslated, so the kernel's file-type numbers are
/// checked here against the pinned `Interop.Stat.cs` too.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileStatusPal =

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)>]
    extern int private hostShimLStat(string path, byte[] output)

    /// Darwin's `chflags(2)`. A `DllImport` binds on first call, so naming a
    /// symbol a Linux host lacks costs nothing until something calls it.
    [<DllImport("libc", EntryPoint = "chflags", SetLastError = true)>]
    extern int private hostChflags(string path, uint32 flags)

    let private pinnedHeader () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir ->

        let path = Path.Combine (dir, "src", "native", "libs", "System.Native", "pal_io.h")

        if not (File.Exists path) then
            failwith
                $"TestFileStatusPal: expected the pinned PAL header at %s{path}. If the sparse checkout in flake.nix no longer includes it, this transcription has lost its oracle."

        File.ReadAllText path

    /// `internal const int S_IFDIR = 0x4000;` and friends.
    let private fileTypeEntry : Regex =
        Regex (@"internal const int (?<name>S_IF[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+);")

    [<Test>]
    let ``PAL_UF_HIDDEN is upstream's`` () : unit =
        let m =
            Regex.Match (pinnedHeader (), @"PAL_UF_HIDDEN\s*=\s*0x(?<value>[0-9A-Fa-f]+)")

        if not m.Success then
            failwith "TestFileStatusPal: found no PAL_UF_HIDDEN in the pinned pal_io.h; teach this test its new shape."

        FileStatusPal.palHidden
        |> shouldEqual (Convert.ToUInt32 (m.Groups.["value"].Value, 16))

    [<Test>]
    let ``a flavour with no st_flags writes no UserFlags`` () : unit =
        FileStatusPal.userFlags None |> shouldEqual 0u

    /// The flags this test sets on a fresh file: none, `UF_NODUMP` (1),
    /// `UF_HIDDEN` (0x8000) and both. Each is one a file's owner may set and
    /// clear again, which the test must do to delete it.
    let private flagRows : uint32 list = [ 0u ; 0x1u ; 0x8000u ; 0x8001u ]

    [<Test>]
    let ``the UserFlags written for each st_flags are what this host's shim writes`` () : unit =
        HostStat.onMeasuredHost (fun flavour ->
            match flavour with
            | SimulatedUnixFlavour.Linux ->
                Assert.Ignore "a Linux host's struct stat has no st_flags to set; the Linux row is the unit test above"
            | SimulatedUnixFlavour.Darwin ->

            for flags in flagRows do
                let unique = Guid.NewGuid().ToString "N"
                let path = Path.Combine (Path.GetTempPath (), $"pawprint-flags-%s{unique}")
                File.Create(path).Dispose ()

                try
                    if hostChflags (path, flags) <> 0 then
                        failwith $"host chflags 0x%x{flags} failed: errno %d{Marshal.GetLastPInvokeError ()}"

                    // The flags the kernel then reports, so a refused bit is
                    // not mistaken for one the shim discarded.
                    let reported = (HostStat.lstat flavour path).FileFlags
                    reported |> shouldEqual (Some flags)

                    let output = Array.zeroCreate<byte> 256

                    if hostShimLStat (path, output) <> 0 then
                        failwith $"host SystemNative_LStat failed: errno %d{Marshal.GetLastPInvokeError ()}"

                    // `UserFlags` is the `uint32_t` at 112 of the shim's
                    // 120-byte `FileStatus`.
                    let shim = BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan (output, 112, 4))

                    FileStatusPal.userFlags reported |> shouldEqual shim
                finally
                    hostChflags (path, 0u) |> ignore<int>
                    File.Delete path
        )

    [<Test>]
    let ``the kernel's S_IFMT band agrees with the pinned Interop.Stat.cs`` () : unit =
        // `fileTypeBits` is where the kernel decides what `st_mode & S_IFMT`
        // says, and the FileStatus PawPrint writes carries that mode
        // untranslated. Checking it against a second copy of the same literals
        // would prove nothing, so the oracle is upstream's own declaration:
        // the very numbers the guest's CoreLib will compare against.
        let path =
            match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
            | null
            | "" ->
                Assert.Ignore
                    "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

                failwith "unreachable: Assert.Ignore did not throw"
            | dir ->
                Path.Combine (
                    dir,
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
                $"expected the pinned FileStatus declaration at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common/src/Interop/Unix/System.Native, InodeContent.fileTypeBits has lost its oracle."

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

        InodeContent.fileTypeBits (
            InodeContent.RegularFile (ImmutableArray<byte>.Empty, SeedEntry.defaultPermsForRegularFile)
        )
        |> shouldEqual (ofName "S_IFREG")

        InodeContent.fileTypeBits (
            InodeContent.Directory
                {
                    Entries = Map.empty
                    Parent = InodeNumber 1L
                    Permissions = SeedEntry.defaultPermsForDirectory
                }
        )
        |> shouldEqual (ofName "S_IFDIR")

        InodeContent.fileTypeBits (
            InodeContent.Symlink (SymlinkTarget.parseOrFail "test" "x", (PermissionBits.parseOrFail "test" 0o777))
        )
        |> shouldEqual (ofName "S_IFLNK")

        // ...and each of them really is inside the band, so that a value that
        // happened to match a typo'd constant still could not be a plausible
        // file type.
        let mask = ofName "S_IFMT"

        for content in
            [
                InodeContent.RegularFile (ImmutableArray<byte>.Empty, SeedEntry.defaultPermsForRegularFile)
                InodeContent.Symlink (SymlinkTarget.parseOrFail "test" "x", (PermissionBits.parseOrFail "test" 0o777))
            ] do
            let bits = InodeContent.fileTypeBits content
            bits &&& mask |> shouldEqual bits
