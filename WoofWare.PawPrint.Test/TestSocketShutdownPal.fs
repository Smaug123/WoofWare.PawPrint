namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `SocketShutdownPal` transcribes `pal_networking_common.h`'s `SocketShutdown`
/// and the switch of `Common_Shutdown` that converts it, so its oracles are
/// upstream's source, pinned, and the host's own shim, whose
/// `SystemNative_Shutdown` answers `Error_EINVAL` for exactly the values it
/// does not convert, before it looks at the descriptor.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketShutdownPal =

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private commonSource () : string =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"
        | dir ->
            let path =
                Path.Combine (dir, "src", "native", "libs", "Common", "pal_networking_common.h")

            if not (File.Exists path) then
                failwith
                    $"TestSocketShutdownPal: expected the pinned PAL networking source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/Common, this transcription has lost its oracle."

            File.ReadAllText path

    /// `shutdown(2)`'s `how` by name, the same on every flavour.
    let private shutHow (name : string) : int =
        match name with
        | "SHUT_RD" -> 0
        | "SHUT_WR" -> 1
        | "SHUT_RDWR" -> 2
        | other -> failwith $"no shutdown how named %s{other}"

    [<Test>]
    let ``the PAL's SocketShutdown values convert as upstream's Common_Shutdown converts them`` () : unit =
        let source = commonSource ()

        let values =
            Regex.Matches (
                source,
                @"^\s+SocketShutdown_(?<name>SHUT_[A-Z]+)\s*=\s*(?<value>\d+),",
                RegexOptions.Multiline
            )
            |> Seq.map (fun m -> m.Groups.["name"].Value, int m.Groups.["value"].Value)
            |> Map.ofSeq

        let conversions =
            Regex.Matches (source, @"case\s+SocketShutdown_(?<pal>SHUT_[A-Z]+):\s*how\s*=\s*(?<how>SHUT_[A-Z]+);")
            |> Seq.map (fun m -> values.[m.Groups.["pal"].Value], shutHow m.Groups.["how"].Value)
            |> List.ofSeq

        conversions.Length |> shouldEqual 3

        for pal, how in conversions do
            SocketShutdownPal.toPlatform pal |> shouldEqual (Some how)

        // Every other value is the shim's EINVAL.
        for pal in [ -1 ; 3 ; 4 ; Int32.MaxValue ; Int32.MinValue ] do
            SocketShutdownPal.toPlatform pal |> shouldEqual None

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Shutdown")>]
    extern int private hostShutdown(nativeint socket, int socketShutdown)

    [<Test>]
    let ``the host's shim answers EINVAL for exactly the values it does not convert, before the descriptor`` () : unit =
        if
            not (
                RuntimeInformation.IsOSPlatform OSPlatform.Linux
                || RuntimeInformation.IsOSPlatform OSPlatform.OSX
            )
        then
            Assert.Ignore "no System.Native shim to measure on this host"

        let einval = UnixErrorPal.toPal WoofWare.PosixKernel.UnixError.EINVAL
        let ebadf = UnixErrorPal.toPal WoofWare.PosixKernel.UnixError.EBADF

        // A descriptor no process holds: the converted values reach the
        // kernel, which answers EBADF; the others never do.
        for pal in [ -2 ; -1 ; 0 ; 1 ; 2 ; 3 ; 4 ; 100 ] do
            let expected =
                match SocketShutdownPal.toPlatform pal with
                | Some _ -> ebadf
                | None -> einval

            hostShutdown (-1n, pal) |> shouldEqual expected
