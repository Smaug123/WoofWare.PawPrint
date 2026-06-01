namespace WoofWare.PawPrint.Test

open System
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PawPrint

/// `EmulatedRuntime.current` is a hand-maintained descriptor of which .NET servicing version
/// PawPrint reproduces. The test suite runs on the runtime pinned by the Nix devshell, so if that
/// pin moves ahead of the descriptor — as happens when nixpkgs bumps the SDK — this test fails
/// loudly rather than letting a stale version ship. See `EmulatedRuntime.fs`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedRuntime =

    /// The servicing version of the runtime actually executing this test, parsed out of e.g.
    /// ".NET 10.0.7" (dropping any prerelease suffix such as "-preview.1.…").
    let private runningRuntimeVersion () : Version =
        let token = RuntimeInformation.FrameworkDescription.Split ' ' |> Array.last
        Version.Parse ((token.Split '-').[0])

    [<Test>]
    let ``EmulatedRuntime.current matches the runtime under test`` () =
        let running = runningRuntimeVersion ()
        let claimed = EmulatedRuntime.current.Version

        // Compare to servicing (Major.Minor.Build) granularity; the constant is written with three
        // components, and FrameworkDescription reports three.
        if
            (claimed.Major, claimed.Minor, claimed.Build)
            <> (running.Major, running.Minor, running.Build)
        then
            failwithf
                "EmulatedRuntime.current.Version (%O) disagrees with the runtime under test (%O, from FrameworkDescription %A). Update WoofWare.PawPrint/EmulatedRuntime.fs (and re-sync the dotnet-runtime reference) to match the devshell-pinned runtime."
                claimed
                running
                RuntimeInformation.FrameworkDescription
