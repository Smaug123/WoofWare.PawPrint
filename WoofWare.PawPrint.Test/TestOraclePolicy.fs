namespace WoofWare.Pawprint.Test

open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `OraclePolicy` decides which end-to-end cases are compared against the real
/// runtime on the host at hand. Three things are worth pinning: the decision itself,
/// that the decision is actually consulted by the harness, and the environmental
/// premises a comparison rests on once it has been decided on -- a host that fails
/// one of those still runs the comparison, and the comparison still passes, having
/// measured nothing.
[<TestFixture>]
module TestOraclePolicy =

    /// The real export, in the shim this test host runs against. Declared here as
    /// well as in `WoofWare.PosixKernel.Test`'s `TestFileSystemType`, which reads it
    /// to check the model against: a `private extern` cannot cross a project, and
    /// the two ask different questions of the same symbol.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetFileSystemType", SetLastError = true)>]
    extern uint32 private hostGetFileSystemType(nativeint fd)

    // Short names purely so the table below reads as a table.
    let private linux : SimulatedUnixFlavour = SimulatedUnixFlavour.Linux
    let private darwin : SimulatedUnixFlavour = SimulatedUnixFlavour.Darwin
    let private always : OraclePolicy = OraclePolicy.Always
    let private never : OraclePolicy = OraclePolicy.Never
    let private matching : OraclePolicy = OraclePolicy.WhenHostMatchesEmulatedFlavour

    /// Every (host, impersonated kernel, policy) triple and the answer it must give,
    /// written out rather than recomputed: an expectation derived from the same
    /// formula as the implementation would agree with a wrong implementation.
    ///
    /// `None` is a host PawPrint models no flavour for — Windows — which matches
    /// nothing, so a platform-specific case there falls back to its PawPrint-only
    /// assertion.
    let private truthTable : (SimulatedUnixFlavour option * SimulatedUnixFlavour * OraclePolicy * bool) list =
        [
            None, linux, always, true
            None, darwin, always, true
            Some linux, linux, always, true
            Some linux, darwin, always, true
            Some darwin, linux, always, true
            Some darwin, darwin, always, true

            None, linux, never, false
            None, darwin, never, false
            Some linux, linux, never, false
            Some linux, darwin, never, false
            Some darwin, linux, never, false
            Some darwin, darwin, never, false

            None, linux, matching, false
            None, darwin, matching, false
            Some linux, linux, matching, true
            Some linux, darwin, matching, false
            Some darwin, linux, matching, false
            Some darwin, darwin, matching, true
        ]

    [<Test>]
    let ``comparesOnHost agrees with the table`` () : unit =
        for host, impersonated, policy, expected in truthTable do
            let actual = OraclePolicy.comparesOnHost host impersonated policy

            if actual <> expected then
                failwith
                    $"comparesOnHost (host %O{host}) (impersonating %O{impersonated}) %O{policy} answered %b{actual}, but the table says %b{expected}."

    /// Host width and byte order, and whether a host of that shape can be an oracle
    /// for either preset. Both presets describe a 64-bit little-endian kernel.
    let private shapeTable : (bool * int * bool) list =
        [ true, 8, true ; true, 4, false ; false, 8, false ; false, 4, false ]

    [<Test>]
    let ``only a 64-bit little-endian host can stand in for a preset`` () : unit =
        for isLittleEndian, pointerSize, expected in shapeTable do
            let actual = OraclePolicy.hostShapeCanCompare isLittleEndian pointerSize

            if actual <> expected then
                failwith
                    $"hostShapeCanCompare (littleEndian %b{isLittleEndian}) (pointer %d{pointerSize} bytes) answered %b{actual}, but the table says %b{expected}."

    [<Test>]
    let ``this host's shape is described by the presets`` () : unit =
        // Not a tautology restating the function: it says that the machine running the
        // suite is one the compared cases are actually compared on, so a green run here
        // is evidence about the oracle rather than about a silent fallback.
        OraclePolicy.hostShapeCanCompare System.BitConverter.IsLittleEndian System.IntPtr.Size
        |> shouldEqual true

    [<Test>]
    let ``the table covers every combination`` () : unit =
        // Both axes are closed, so the table can be complete rather than merely
        // representative; a policy case added later fails here until it is described.
        truthTable |> List.length |> shouldEqual 18

        truthTable
        |> List.distinctBy (fun (h, i, p, _) -> h, i, p)
        |> List.length
        |> shouldEqual 18

    [<Test>]
    let ``comparesHere reads the case's own impersonated kernel`` () : unit =
        // Not "the default kernel" and not a flavour written down beside the policy:
        // the flavour compared against is the one the case tells PawPrint to
        // impersonate, so a case that changes its KernelConfig changes when it is
        // compared, with nothing else to keep in step.
        let case (platform : SimulatedUnixPlatform) : EndToEndTestCase =
            {
                FileName = "TestOraclePolicy-synthetic"
                ExpectedReturnCode = 0
                KernelConfig =
                    { KernelConfig.Default with
                        UnixPlatform = platform
                    }
                AppContext = AppContextProperties.empty
                Oracle = OraclePolicy.WhenHostMatchesEmulatedFlavour
                ExpectsUnhandledException = false
                AssertTerminalState = None
            }

        match HostPlatform.flavour () with
        | None ->
            OraclePolicy.comparesHere (case SimulatedUnixPlatform.linuxX64)
            |> shouldEqual false

            OraclePolicy.comparesHere (case SimulatedUnixPlatform.macOsArm64)
            |> shouldEqual false
        | Some SimulatedUnixFlavour.Linux ->
            OraclePolicy.comparesHere (case SimulatedUnixPlatform.linuxX64)
            |> shouldEqual true

            OraclePolicy.comparesHere (case SimulatedUnixPlatform.macOsArm64)
            |> shouldEqual false
        | Some SimulatedUnixFlavour.Darwin ->
            OraclePolicy.comparesHere (case SimulatedUnixPlatform.linuxX64)
            |> shouldEqual false

            OraclePolicy.comparesHere (case SimulatedUnixPlatform.macOsArm64)
            |> shouldEqual true

    let private comparedCases : EndToEndTestCase list =
        TestImpureCases.cases
        |> List.filter (fun case -> case.Oracle = OraclePolicy.WhenHostMatchesEmulatedFlavour)

    [<Test>]
    let ``both flavours have a compared case`` () : unit =
        // The point of the policy is that CI (Linux) and a macOS dev box each get a
        // real oracle for the half of the corpus that describes their own kernel. If
        // either half were empty the mechanism would be dead on that host and nothing
        // else in the suite would say so.
        let flavours =
            comparedCases
            |> List.map (fun case -> SimulatedUnixPlatform.flavour case.KernelConfig.UnixPlatform)
            |> Set.ofList

        flavours
        |> shouldEqual (Set.ofList [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ])

    [<Test>]
    let ``compared cases seed no AppContext properties`` () : unit =
        // `DifferentialOracle.assertComparable` refuses this per case as it runs; asked
        // here of the whole corpus at once, so that registering an uncomparable case is
        // caught even on a host that would not have compared it.
        comparedCases
        |> List.filter (fun case -> not (AppContextProperties.isEmpty case.AppContext))
        |> List.map (fun case -> case.FileName)
        |> shouldEqual []

    [<Test>]
    let ``no impure case expects an unhandled exception`` () : unit =
        // The impure fixture reads an exit code off the terminating thread, which a
        // guest that died of an escaping exception never produces, so it refuses the
        // declaration outright. Asked here of the whole corpus rather than only of the
        // case that happens to run, and asked of every case rather than only the
        // compared ones, because the refusal is about this fixture and not the oracle.
        TestImpureCases.cases @ TestImpureCases.unimplemented
        |> List.filter (fun case -> case.ExpectsUnhandledException)
        |> List.map (fun case -> case.FileName)
        |> shouldEqual []

    [<Test>]
    let ``the impure harness refuses a case that expects an unhandled exception`` () : unit =
        // The corpus assertion above says no case does this today; this says what
        // happens to the one that tries. Refused before the guest runs, so the
        // diagnostic does not depend on how far the interpreter got.
        let case =
            TestImpureCases.cases
            |> List.find (fun case -> case.FileName = "AssemblyLocationEmpty.cs")

        let forced =
            { case with
                ExpectsUnhandledException = true
            }

        let exn = Assert.Throws<System.Exception> (fun () -> TestImpureCases.runTest forced)

        exn.Message |> shouldContainText "AssemblyLocationEmpty.cs"
        exn.Message |> shouldContainText "ExpectsUnhandledException"

    [<Test>]
    let ``a divergent case registered as compared is caught`` () : unit =
        // The wiring, not the decision: proof that `TestImpureCases.runTest` really does
        // consult the policy and really does run the oracle when it says so.
        //
        // `AssemblyLocationEmpty.cs` is a documented divergence — PawPrint reports every
        // assembly's `Location` as empty, while the real runtime is launched from a real
        // .dll and reports its path, so the guest returns 0 here and 2 there. Registered
        // honestly it is `Never`; forced to compare, the harness must notice. A harness
        // that ignored the policy, or ran the oracle and discarded its answer, would let
        // this pass.
        let divergent =
            TestImpureCases.cases
            |> List.find (fun case -> case.FileName = "AssemblyLocationEmpty.cs")

        divergent.Oracle |> shouldEqual OraclePolicy.Never

        // `Always` rather than `WhenHostMatchesEmulatedFlavour` so that the assertion
        // holds on every host: which branch of `comparesOnHost` returned true is the
        // truth table's business, and the two policies reach `runTest`'s oracle call
        // through the same line.
        let forced =
            { divergent with
                Oracle = OraclePolicy.Always
            }

        let exn = Assert.Throws<System.Exception> (fun () -> TestImpureCases.runTest forced)

        // Names the guest and both runtimes' answers, so a failure that happened to
        // come from somewhere else in the harness would not satisfy this.
        exn.Message |> shouldContainText "AssemblyLocationEmpty.cs"
        exn.Message |> shouldContainText "Real runtime exited with code 2"
        exn.Message |> shouldContainText "ExpectedReturnCode = 0"

    [<Test>]
    let ``the two runtimes' own answers are compared, not just the declared one`` () : unit =
        // The other half of `compareOutcomes`. Above, the real runtime disagreed with
        // the number the case wrote down; here it *agrees* with it, and PawPrint is the
        // one that differs. Nothing in the corpus can exercise this while the suite is
        // green -- a case where PawPrint and the real runtime disagree is a bug, not a
        // fixture -- so the divergence is manufactured by declaring the real runtime's
        // answer as the expected one.
        let divergent =
            TestImpureCases.cases
            |> List.find (fun case -> case.FileName = "AssemblyLocationEmpty.cs")

        let forced =
            { divergent with
                Oracle = OraclePolicy.Always
                // What real .NET answers: `Location` is a real path, so the guest's
                // second check fails there. PawPrint answers 0.
                ExpectedReturnCode = 2
            }

        let exn = Assert.Throws<System.Exception> (fun () -> TestImpureCases.runTest forced)

        exn.Message |> shouldContainText "PawPrint exited with code 0"
        exn.Message |> shouldContainText "the real runtime exited with 2"

    [<Test>]
    let ``this host's own filesystem is one CoreCLR will lock`` () : unit =
        // Not a claim about the model: it is the environmental premise the
        // differential half rests on. `sourcesPure/FlockContentionSeeded.cs`
        // compares PawPrint's locking against the real runtime's, and the real
        // runtime takes a shared lock under write access only when the scratch
        // directory's filesystem is not NFS, CIFS or SMB. On a machine where
        // that failed, those guest checks would pass vacuously against a
        // runtime that locked nothing — so the premise is asserted here, where
        // a failure names the actual cause.
        match HostPlatform.flavour () with
        | None -> Assert.Ignore $"no Unix shim to measure (%s{RuntimeInformation.OSDescription})"
        | Some _ ->

        let path = System.IO.Path.GetTempFileName ()

        try
            use handle =
                System.IO.File.OpenHandle (path, System.IO.FileMode.Open, System.IO.FileAccess.Read)

            let hostSaid = hostGetFileSystemType (handle.DangerousGetHandle ())

            // The four `SafeFileHandle.CanLockTheFile` refuses, plus 0, which it
            // treats as "unknown, so do not lock".
            let unlockable =
                Map.ofList
                    [
                        0u, "an unknown filesystem"
                        0x6969u, "nfs"
                        0x517Bu, "smb"
                        0xFE534D42u, "smb2"
                        0xFF534D42u, "cifs"
                    ]

            match Map.tryFind hostSaid unlockable with
            | None -> ()
            | Some name ->
                failwith
                    $"this host's temporary directory is on %s{name} (0x%X{hostSaid}), where CoreCLR declines to take a shared lock under write access. FlockContentionSeeded.cs's write-access checks would pass vacuously here."
        finally
            System.IO.File.Delete path
