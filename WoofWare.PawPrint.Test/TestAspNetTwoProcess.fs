namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text
open FsUnitTyped
open Microsoft.CodeAnalysis
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PawPrint.Test.LinuxCoreLibFlavour

/// An ASP.NET Core server and an `HttpClient` client, each its own process on one simulated
/// machine, talking over loopback TCP; and the same two images as two real .NET processes.
///
/// The server (`sourcesAspNet/AspNetTwoProcessServer.cs`) is a minimal-API app on Kestrel's
/// socket transport, listening on a port the kernel chooses. It hands its address to the client
/// through a file it renames into place, and the client (`AspNetTwoProcessClient.cs`) waits for
/// that file. The client makes a GET with a query, a POST with a JSON body and a request that
/// must 404, then asks the server to stop; the server stops and checks it served both item
/// requests. Each guest exits 42 only if its own checks held.
///
/// What this claims: under PawPrint both guests exit 42; under the real runtime both exit 42 too
/// (a precondition, so that two failing runs cannot pass as agreement); each guest wrote the same
/// standard output on both sides, which names no port; and once both processes have ended the
/// machine holds no process and its invariants hold.
///
/// What it does not claim:
/// - **The Linux CoreLib only.** PawPrint runs the pinned linux-x64 framework
///   (`$DOTNET_LINUX_FRAMEWORK_DIR`), and the test is ignored where that is unset. ASP.NET Core
///   itself binds from the framework under test, since the pack holds only
///   `Microsoft.NETCore.App`. On a macOS CoreLib, `HttpClient` asks CFNetwork for the system
///   proxy settings, which PawPrint does not implement. The real run uses the host's own
///   CoreLib, so on macOS the comparison is between flavours, of facts that hold on both.
/// - **The polling file watcher.** Both sides run with `DOTNET_USE_POLLING_FILE_WATCHER=1`,
///   because PawPrint does not implement inotify, which the configuration's reload-on-change
///   watcher uses otherwise.
/// - **A proxy.** Both sides run with every proxy variable `HttpClient` reads set empty, so
///   that the real client cannot inherit one from the host.
/// - **One interleaving.** The two programs take turns round robin. In it the client ends while
///   its keep-alive connection is open, and the server then stops; the reverse order, the
///   server closing an idle connection under a live client, is not exercised.
/// - **No refused connection.** The client connects only once the server has written its
///   address. `TestSeveralPrograms` covers a connector retrying on refusal.
/// - **A Roslyn compilation, not a Web SDK publish.** Both sides get PawPrint's own AppContext
///   baseline (`AppContextProperties.runtimeBaseline`, so no dynamic code) and invariant
///   globalization, and nothing else from a Web SDK runtimeconfig.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it `Explicit`, and CI
// selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestAspNetTwoProcess =

    /// The handoff file's name, relative to each process's working directory: `/` under
    /// PawPrint, and the oracle's scratch directory under the real runtime.
    let private handoff : string = "server-address"

    /// The environment of both processes, on both sides. The real processes otherwise inherit
    /// the test host's environment, so every variable from which `HttpClient` reads a proxy is
    /// set here, empty, which it reads as no proxy: a proxy the host configures must not carry
    /// the real client's loopback requests away from the server.
    let private environment : string list =
        [
            EnvironmentPal.nameValueEntry "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT" "1"
            EnvironmentPal.nameValueEntry "DOTNET_USE_POLLING_FILE_WATCHER" "1"
            for name in
                [
                    "http_proxy"
                    "HTTP_PROXY"
                    "https_proxy"
                    "HTTPS_PROXY"
                    "all_proxy"
                    "ALL_PROXY"
                    "no_proxy"
                    "NO_PROXY"
                ] do
                EnvironmentPal.nameValueEntry name ""
        ]

    /// The AppContext properties of both processes, on both sides.
    let private appContext : AppContextProperties = AppContextProperties.runtimeBaseline

    // Made once: each reference holds a copy of its image in native memory (see `Roslyn`).
    let private aspNetCoreReferences : Lazy<MetadataReference list> =
        lazy
            (Directory.GetFiles (FrameworkUnderTest.aspNetCoreFrameworkDirectory (), "*.dll")
             |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)
             |> Array.toList)

    let private compile (assemblyName : string) : byte[] =
        let source =
            Assembly.getEmbeddedResourceAsString (assemblyName + ".cs") typeof<RunResult>.Assembly

        Roslyn.compileAssembly assemblyName OutputKind.ConsoleApplication (aspNetCoreReferences.Force ()) [ source ]

    let private server : Lazy<byte[]> = lazy (compile "AspNetTwoProcessServer")
    let private client : Lazy<byte[]> = lazy (compile "AspNetTwoProcessClient")

    /// How one guest ended on one side: its exit code, or how it failed to exit; and what it wrote.
    type private Ending =
        {
            Exit : Result<int, string>
            Stdout : string
            Stderr : string
        }

    let private describe (ending : Ending) : string =
        let how =
            match ending.Exit with
            | Ok code -> $"exited %d{code}"
            | Error how -> how

        $"%s{how}\nstdout:\n%s{ending.Stdout}\nstderr:\n%s{ending.Stderr}"

    let private ofCapturedRun (run : CapturedRun) : Ending =
        {
            Exit =
                match run.Result with
                | RealRuntimeResult.NormalExit code -> Ok code
                | RealRuntimeResult.UnhandledException report -> Error $"ended by an unhandled exception:\n%s{report}"
                | RealRuntimeResult.Aborted (observed, report) -> Error $"aborted (%O{observed}):\n%s{report}"
            Stdout = run.Stdout
            Stderr = run.Stderr
        }

    let private ofRunEnd (runEnd : RunEnd) : Ending =
        let state = RunEnd.state runEnd

        let text (role : FileDescriptorRole) : string =
            state.Kernel.OutputLog
            |> OutputLogEntry.bytesFor role
            |> Seq.toArray
            |> Encoding.UTF8.GetString

        // Each case by what it says, never by `%O`, which would print the whole machine.
        let exit =
            match runEnd with
            | RunEnd.Ended (RunOutcome.NormalExit (state, _, _))
            | RunEnd.Ended (RunOutcome.ProcessExit (state, _, _)) -> Ok state.LatchedExitCode
            | RunEnd.Ended (RunOutcome.GuestUnhandledException (state, _, exn, _)) ->
                Error $"ended by an unhandled exception:\n%s{UnhandledExceptionReport.describe state exn}"
            | RunEnd.Ended (RunOutcome.Aborted (_, _, fatal, _)) ->
                let message = fatal.Message |> Option.defaultValue "<no message>"
                Error $"aborted (%O{fatal.Code}): %s{message}"
            | RunEnd.Ended (RunOutcome.SignalTerminated (_, signal, coreDumped)) ->
                Error $"was killed by %O{signal} (core dumped: %b{coreDumped})"

        {
            Exit = exit
            Stdout = text FileDescriptorRole.StandardOutput
            Stderr = text FileDescriptorRole.StandardError
        }

    /// Both guests under PawPrint, on the Linux CoreLib in `linuxFramework`, as two processes of
    /// one machine; failing unless the machine is left with no process and its invariants hold.
    let private runUnderPawPrint (linuxFramework : string) : Ending * Ending =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestAspNetTwoProcess"

        let runtimeDirs = runtimeDirsPreferringLinux linuxFramework

        let machineConfig, processConfig =
            KernelConfig.split
                { KernelConfig.Default with
                    Environment = environment
                }

        let launch (name : string) (image : byte[]) : ProgramLaunch =
            {
                Image = new MemoryStream (image)
                OriginalPath = Some (name + ".dll")
                DotnetRuntimeDirs = runtimeDirs
                Process = processConfig
                Argv = [ handoff ]
                AssemblyPath = None
                AppContext = appContext
                PctSeed = None
            }

        let rec go (driver : MultiProgram) : (ProcessId * RunEnd) list =
            match MultiProgram.step loggerFactory logger driver with
            | MultiStepOutcome.Stepped (driver, _, _) -> go driver
            | MultiStepOutcome.Finished (ends, machine) ->
                match SimulatedMachine.processIds machine |> Set.toList with
                | [] -> ()
                | left -> failwith $"PawPrint: both programs ended, but the machine still holds processes %A{left}"

                match SimulatedMachine.checkInvariants machine with
                | [] -> ()
                | broken ->
                    failwith $"PawPrint: both programs ended, but the machine's invariants do not hold: %A{broken}"

                ends
            | MultiStepOutcome.Deadlocked (_, stuck) ->
                failwith $"PawPrint: the server and the client deadlocked. Where each program was: %A{stuck}"

        let driver =
            MultiProgram.start
                loggerFactory
                { machineConfig with
                    ProgramChoice = ProgramChoice.RoundRobin
                }
                [
                    launch "AspNetTwoProcessServer" (server.Force ())
                    launch "AspNetTwoProcessClient" (client.Force ())
                ]

        match go driver with
        | [ (_, serverEnd) ; (_, clientEnd) ] ->
            // Not just that the runtime-dir list began with the pack: the server really bound its
            // CoreLib from it.
            let corelib = loadedCorelibPath (RunEnd.state serverEnd)

            if not (corelib.StartsWith (linuxFramework, StringComparison.Ordinal)) then
                failwith
                    $"PawPrint: the server ran on the CoreLib at %s{corelib}, not the Linux one in %s{linuxFramework}"

            ofRunEnd serverEnd, ofRunEnd clientEnd
        | other -> failwith $"PawPrint: expected the server's and the client's ends, got %d{other.Length}"

    /// Both guests as two concurrent processes on the real runtime.
    let private runUnderRealRuntime () : Ending * Ending =
        let runs =
            RealRuntime.executeConcurrently
                OracleFrameworks.NetCoreAppAndAspNetCoreApp
                appContext
                environment
                [
                    {
                        Image = server.Force ()
                        Args = [ handoff ]
                    }
                    {
                        Image = client.Force ()
                        Args = [ handoff ]
                    }
                ]

        match runs with
        | [ serverRun ; clientRun ] -> ofCapturedRun serverRun, ofCapturedRun clientRun
        | other -> failwith $"The real runtime: expected the server's and the client's runs, got %d{other.Length}"

    /// What went wrong with `half`, unless it exited 42.
    let private failure (half : string) (ending : Ending) : string option =
        match ending.Exit with
        | Ok 42 -> None
        | _ -> Some $"the %s{half} did not exit 42; it %s{describe ending}"

    /// Each half that did not exit 42, and how it ended instead.
    let private failures (server : Ending) (client : Ending) : string list =
        List.choose id [ failure "server" server ; failure "client" client ]

    [<Test>]
    let ``Kestrel and HttpClient as two processes on one machine agree with two real processes`` () : unit =
        let linuxFramework = requireLinuxFramework ()

        let (realServer, realClient), (pawPrintServer, pawPrintClient) =
            DifferentialOracle.alongsideInterpreted runUnderRealRuntime (fun () -> runUnderPawPrint linuxFramework)

        // Every half that failed is named, rather than the first: one half's failure usually
        // makes the other fail too (a client that gives up never asks the server to stop), and
        // the first to be checked need not be the cause.
        let summary (failed : string list) : string =
            match failed with
            | [] -> "both exited 42"
            | failed -> String.concat "\n\n" failed

        // The oracle first: if the real runs failed, PawPrint has nothing to agree with.
        match failures realServer realClient with
        | [] -> ()
        | failed ->
            failwith
                $"The real runtime: %s{summary failed}\n\nSo PawPrint's run, in which %s{summary (failures pawPrintServer pawPrintClient)}, was not compared."

        match failures pawPrintServer pawPrintClient with
        | [] -> ()
        | failed -> failwith $"PawPrint: %s{summary failed}"

        let compareStdout (half : string) (real : Ending) (pawPrint : Ending) : unit =
            if real.Stdout <> pawPrint.Stdout then
                failwith
                    $"The %s{half} wrote different standard output under PawPrint and under the real runtime.\nPawPrint:\n%s{pawPrint.Stdout}\nThe real runtime:\n%s{real.Stdout}"

        compareStdout "server" realServer pawPrintServer
        compareStdout "client" realClient pawPrintClient

        // Each guest's own checks wrote to standard output as they passed, so agreement means
        // every one of them ran on both sides.
        pawPrintClient.Stdout
        |> shouldEqual (
            String.concat
                ""
                [
                    "client: found the server\n"
                    "client: GET answered\n"
                    "client: POST answered\n"
                    "client: missing item answered 404\n"
                    "client: asked the server to stop\n"
                ]
        )

        pawPrintServer.Stdout
        |> shouldEqual "server: listening\nserver: stopped after serving both item requests\n"
