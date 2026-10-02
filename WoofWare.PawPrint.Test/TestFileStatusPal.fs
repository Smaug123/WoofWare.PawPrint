namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
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
/// over files whose flags this test sets.
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
