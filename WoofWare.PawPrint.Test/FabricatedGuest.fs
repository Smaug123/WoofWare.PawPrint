namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open WoofWare.PawPrint

/// How the driver terminated under PawPrint.
[<RequireQualifiedAccess>]
type FabricatedOutcome =
    /// The guest ran to completion and returned this exit code.
    | Exited of int
    /// PawPrint refused to run the guest, or failed while running it. Most of PawPrint's refusals
    /// are `failwith`s that escape `Program.run`; the outcomes it reports rather than throws are
    /// raised here instead, so that every failure carries the captured log to stderr and a caller
    /// has one case to match on.
    | Failed of exn

/// Runs a guest whose IL no C# source can spell: a fabricated assembly (built with
/// `PersistedAssemblyBuilder`, so its method bodies are whatever IL the fixture emits) plus a
/// Roslyn-compiled C# driver that calls into it.
[<RequireQualifiedAccess>]
module FabricatedGuest =

    /// Lay `fabricated` and a driver compiled against it side by side in a fresh directory, and
    /// hand `run` that directory, the driver's path and the driver's image. The directory is removed
    /// afterwards.
    let private laidOut
        (fabricatedName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        (run : string -> string -> byte[] -> 'a)
        : 'a
        =
        let driver =
            Roslyn.compileAssembly
                driverName
                OutputKind.ConsoleApplication
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange fabricated) ]
                [ driverSource ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (tempDir, $"%s{fabricatedName}.dll"), fabricated)
            let driverPath = Path.Combine (tempDir, $"%s{driverName}.dll")
            File.WriteAllBytes (driverPath, driver)
            run tempDir driverPath driver
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    /// PawPrint's runtime directories for a driver laid out in `tempDir`: that directory first, so
    /// the fabricated assembly resolves from it, then the framework under test.
    let private runtimeDirs (tempDir : string) : ImmutableArray<string> =
        seq {
            yield tempDir
            yield! FrameworkUnderTest.runtimeDirs ()
        }
        |> ImmutableArray.CreateRange

    /// PawPrint's outcome for a run, with every failure caught, and the captured log written to
    /// stderr when there is one.
    let private onPawPrint (messages : unit -> 'message list) (run : unit -> RunOutcome) : FabricatedOutcome =
        try
            let outcome = run ()
            FrameworkUnderTest.assertOutcomeServes outcome

            match outcome with
            | RunOutcome.NormalExit (state, _)
            | RunOutcome.ProcessExit (state, _) -> FabricatedOutcome.Exited state.LatchedExitCode
            | RunOutcome.GuestUnhandledException (finalState, _, exn) ->
                failwith $"guest threw:\n%s{UnhandledExceptionReport.describe finalState exn}"
            | RunOutcome.Aborted (_, _, fatal) ->
                let message = fatal.Message |> Option.defaultValue "<none>"
                failwith $"guest aborted (%O{fatal.Code}): %s{message}"
            | RunOutcome.SignalTerminated (_, signal) -> failwith $"guest was signalled: %O{signal}"
        with e ->
            for message in messages () do
                Console.Error.WriteLine $"{message}"

            FabricatedOutcome.Failed e

    /// Lay `fabricated` and a driver compiled against it side by side on disk, and run the driver
    /// on both runtimes.
    let runOnBoth
        (fabricatedName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        : RealRuntimeResult * FabricatedOutcome
        =
        laidOut
            fabricatedName
            fabricated
            driverName
            driverSource
            (fun tempDir driverPath driver ->
                let onHost = RealRuntime.executeAssemblyInPlace [||] driverPath

                let messages, loggerFactory =
                    LoggerFactory.makeTestWithProperties [ "entry_assembly", driverPath ]

                use _loggerFactoryResource = loggerFactory
                use peImage = new MemoryStream (driver)

                let onPawPrint =
                    onPawPrint
                        messages
                        (fun () ->
                            Program.run
                                loggerFactory
                                (Some driverPath)
                                peImage
                                (HostConfig.Default (runtimeDirs tempDir))
                        )

                onHost, onPawPrint
            )

    /// Run the driver on PawPrint alone, giving up after `maxSteps` interpreted steps. For a guest
    /// the real runtime cannot usefully run, and which PawPrint might never finish.
    let runOnPawPrintBounded
        (fabricatedName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        (maxSteps : int64)
        : FabricatedOutcome
        =
        laidOut
            fabricatedName
            fabricated
            driverName
            driverSource
            (fun tempDir driverPath driver ->
                let messages, loggerFactory =
                    LoggerFactory.makeTestWithProperties [ "entry_assembly", driverPath ]

                use _loggerFactoryResource = loggerFactory
                use peImage = new MemoryStream (driver)

                onPawPrint
                    messages
                    (fun () ->
                        BoundedRun.runWith
                            loggerFactory
                            maxSteps
                            driverName
                            (Some driverPath)
                            peImage
                            (HostConfig.Default (runtimeDirs tempDir))
                    )
            )

    /// Run the driver on both runtimes and require they agree on the exit code.
    ///
    /// `expectedOnHost` is asserted against the real runtime as well as compared with PawPrint, so
    /// a fabrication that stopped exercising the shape it was written for fails here rather than
    /// passing vacuously with both runtimes agreeing on the wrong thing.
    let run
        (fabricatedName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        (expectedOnHost : int)
        : unit
        =
        let onHost, onPawPrint = runOnBoth fabricatedName fabricated driverName driverSource

        let expected =
            match onHost with
            | RealRuntimeResult.NormalExit code -> code
            | other -> failwith $"real runtime did not exit normally: %O{other}"

        expected |> shouldEqual expectedOnHost

        match onPawPrint with
        | FabricatedOutcome.Exited code -> code |> shouldEqual expected
        | FabricatedOutcome.Failed e -> raise (Exception ("PawPrint did not run the guest to completion", e))
