namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open WoofWare.DotnetRuntimeLocator
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

    /// Lay `fabricated` and a driver compiled against it side by side on disk, and run the driver
    /// on both runtimes.
    let runOnBoth
        (fabricatedName : string)
        (fabricated : byte[])
        (driverName : string)
        (driverSource : string)
        : RealRuntimeResult * FabricatedOutcome
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

            let onHost = RealRuntime.executeAssemblyInPlace [||] driverPath

            let messages, loggerFactory =
                LoggerFactory.makeTestWithProperties [ "entry_assembly", driverPath ]

            use _loggerFactoryResource = loggerFactory

            let dotnetRuntimeDirs =
                seq {
                    yield tempDir
                    yield! DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
                }
                |> ImmutableArray.CreateRange

            use peImage = new MemoryStream (driver)

            let onPawPrint =
                try
                    match
                        Program.run loggerFactory (Some driverPath) peImage (HostConfig.Default dotnetRuntimeDirs)
                    with
                    | RunOutcome.NormalExit (state, thread)
                    | RunOutcome.ProcessExit (state, thread) ->
                        match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> FabricatedOutcome.Exited code
                        | [] -> failwith "guest returned no value"
                        | head :: _ -> failwith $"guest returned a non-int: %O{head}"
                    | RunOutcome.GuestUnhandledException (_, _, exn) -> failwith $"guest threw: %O{exn.ExceptionObject}"
                    | RunOutcome.Aborted (_, _, fatal) ->
                        let message = fatal.Message |> Option.defaultValue "<none>"
                        failwith $"guest aborted (%O{fatal.Code}): %s{message}"
                    | RunOutcome.SignalTerminated (_, signal) -> failwith $"guest was signalled: %O{signal}"
                with e ->
                    for message in messages () do
                        Console.Error.WriteLine $"{message}"

                    FabricatedOutcome.Failed e

            onHost, onPawPrint
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

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
