namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open Microsoft.Extensions.Logging
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint.Logging
open WoofWare.PawPrint.ExternImplementations

module Program =
    let reallyMain (argv : string[]) : int =
        let appStaticProperties =
            match argv |> Array.toList with
            | dllPath :: _ -> [ "guest_dll", Path.GetFullPath dllPath ]
            | [] -> []

        use loggerFactory =
            LoggerFactory.Create (fun builder ->
                builder
                    .SetMinimumLevel(PawPrintLogging.minimumLevelFromEnvironment ())
                    .AddConsole (fun options -> options.LogToStandardErrorThreshold <- LogLevel.Trace)
                |> ignore<ILoggingBuilder>

                match PawPrintLogging.tryCreateProviderFromEnvironment "app" "pawprint-app" appStaticProperties with
                | Some provider -> builder.AddProvider provider |> ignore<ILoggingBuilder>
                | None -> ()
            )

        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App"

        match argv |> Array.toList with
        | dllPath :: args ->
            let dotnetRuntimes =
                DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

            let impls = NativeImpls.PassThru ()

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

            let exitCodeFromStack (state : IlMachineState) (thread : ThreadId) : int =
                // Main returned an int32, or Environment.Exit(n) pushed the code on the
                // caller's eval stack before terminating; either way the top-of-stack
                // value is the guest's requested process exit code. If anything else is
                // there (or the stack is empty) we fail loud rather than silently
                // reporting 0 — shell callers depend on the exit code being meaningful.
                match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
                | EvalStackValue.Int32 i :: _ -> i
                | [] -> failwith "Exiting thread returned void; expected an int32 exit code"
                | other :: _ -> failwith $"Exiting thread had unexpected eval-stack top %O{other}; expected int32"

            match Program.run loggerFactory (Some dllPath) fileStream dotnetRuntimes impls args with
            | RunOutcome.NormalExit (state, thread)
            | RunOutcome.ProcessExit (state, thread) -> exitCodeFromStack state thread
            | RunOutcome.GuestUnhandledException (state, _thread, exn) ->
                let exceptionTypeName =
                    match state.ManagedHeap.NonArrayObjects |> Map.tryFind exn.ExceptionObject with
                    | Some obj ->
                        match AllConcreteTypes.lookup obj.ConcreteType state.ConcreteTypes with
                        | Some ti -> $"{ti.Namespace}.{ti.Name}"
                        | None -> $"<unknown type %O{obj.ConcreteType}>"
                    | None -> $"<heap address %O{exn.ExceptionObject}>"

                logger.LogCritical ("Unhandled exception in guest program: {ExceptionTypeName}", exceptionTypeName)

                // On Windows the .NET runtime exits with 0xE0434352 (SEH);
                // on Unix it aborts with SIGABRT (exit code 128 + 6 = 134).
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                    -532462766
                else
                    134
        | _ ->
            logger.LogCritical "Supply exactly one DLL path"
            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
