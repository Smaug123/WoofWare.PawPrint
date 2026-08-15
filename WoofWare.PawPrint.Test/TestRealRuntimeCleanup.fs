namespace WoofWare.PawPrint.Test

open System
open System.IO
open NUnit.Framework
open FsUnitTyped

/// The oracle's scratch directory has to be removable after the run, and a seed
/// may deliberately have left a directory its owner cannot write.
/// `RealRuntime.executeWithTimeoutAndSeed` swallows the delete's exception —
/// rightly, since a leaked temp directory must not fail a test — which is
/// exactly why the guard against it needs a test of its own.
[<TestFixture>]
module TestRealRuntimeCleanup =

    /// A tree shaped like the `FileModeSeeded.cs` seed's awkward corner: a
    /// directory holding a file, with the directory then made unwritable.
    let private buildLockedTree () : string =
        let root =
            Path.Combine (Path.GetTempPath (), "pawprint-cleanup-" + Path.GetRandomFileName ())

        Directory.CreateDirectory root |> ignore<DirectoryInfo>
        let locked = Path.Combine (root, "locked")
        Directory.CreateDirectory locked |> ignore<DirectoryInfo>
        File.WriteAllText (Path.Combine (locked, "inside"), "within")

        // Owner may read and search but not write: enough to see the child,
        // not enough to unlink it.
        File.SetUnixFileMode (
            locked,
            UnixFileMode.UserRead
            ||| UnixFileMode.UserExecute
            ||| UnixFileMode.GroupRead
            ||| UnixFileMode.GroupExecute
            ||| UnixFileMode.OtherRead
            ||| UnixFileMode.OtherExecute
        )

        root

    [<Test>]
    let ``deleteScratchTree removes a tree containing an unwritable directory`` () : unit =
        if System.OperatingSystem.IsWindows () then
            Assert.Ignore "Unix permission bits."

        let root = buildLockedTree ()

        try
            RealRuntime.deleteScratchTree root
            Directory.Exists root |> shouldEqual false
        finally
            if Directory.Exists root then
                RealRuntime.deleteScratchTree root

    /// The control, and the reason the guard exists: without it the delete
    /// fails. A test asserting only that cleanup *works* would pass whether or
    /// not `makeTreeDeletable` did anything.
    ///
    /// Skipped when the tests run as root, which bypasses the permission check
    /// entirely — there the delete would simply succeed and there is nothing to
    /// observe.
    [<Test>]
    let ``a plain recursive delete cannot remove that same tree`` () : unit =
        if System.OperatingSystem.IsWindows () then
            Assert.Ignore "Unix permission bits."

        let root = buildLockedTree ()

        try
            // Whether the delete is refused is decided by *behaviour*, not by
            // asking who we are: an effective uid of 0 bypasses the check, and
            // so does CAP_DAC_OVERRIDE, while `$USER` in a container commonly
            // says neither. So attempt it, and treat success as "this process is
            // privileged, and there is nothing here to observe".
            let refused =
                try
                    Directory.Delete (root, true)
                    false
                with
                // UnauthorizedAccessException, wrapping the IOException the
                // syscall produced — which is why `executeWithTimeoutAndSeed`
                // catches both.
                | :? UnauthorizedAccessException ->
                    true

            if not refused then
                Assert.Ignore
                    "This process can delete through a directory it has no write permission on (effective uid 0, or CAP_DAC_OVERRIDE), so the guard under test has nothing to prevent here."
        finally
            if Directory.Exists root then
                RealRuntime.deleteScratchTree root
