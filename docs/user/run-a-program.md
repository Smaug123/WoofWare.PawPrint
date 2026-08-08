# How to run a program under PawPrint

PawPrint is intended to be used in tests, so is primarily a library.

Use whatever `dotnet build` command you'd usually use to compile an executable DLL, say `/tmp/MyCoolDll.dll`.
For the most predictable results, I'd probably `dotnet publish --self-contained` this; otherwise you may find [WoofWare.DotnetRuntimeLocator](https://github.com/Smaug123/WoofWare.DotnetRuntimeLocator) fails to locate the correct .NET runtime (although it *should* work).

Besides `WoofWare.PawPrint` itself, the example below needs `WoofWare.DotnetRuntimeLocator` (to find the runtime directories to load the BCL from), `Microsoft.Extensions.Logging`, and `Microsoft.Extensions.Logging.Console`:

```xml
<PackageReference Include="WoofWare.DotnetRuntimeLocator" Version="0.3.2" />
<PackageReference Include="Microsoft.Extensions.Logging" Version="9.0.2" />
<PackageReference Include="Microsoft.Extensions.Logging.Console" Version="9.0.2" />
```

```fsharp
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Runs the guest program at `dllPath`, returning the exit code it asked for
/// and everything it wrote to stdout/stderr.
let runGuest (dllPath : string) : int * ImmutableArray<OutputLogEntry> =
    use loggerFactory =
        LoggerFactory.Create (fun builder -> builder.AddConsole () |> ignore<ILoggingBuilder>)

    use peImage = new FileStream (dllPath, FileMode.Open, FileAccess.Read)

    // `SelectForDll` returns a `string seq` of candidate runtime directories.
    let dotnetRuntimes =
        DotnetRuntime.SelectForDll dllPath |> ImmutableArray.CreateRange

    // Everything the host supplies to configure the run. `HostConfig.Default` gives
    // you the default kernel state, the round-robin scheduler, no guest argv, and no
    // AppContext properties; override only what you care about.
    let hostConfig =
        { HostConfig.Default dotnetRuntimes with
            Kernel =
                { KernelConfig.Default with
                    // Whatever you pass here is overlaid on top of
                    // `EmulatedKernel.defaultEnvironment`, so even `Map.empty` gets you
                    // DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1 (invariant globalization is
                    // the only mode PawPrint implements); the keys you supply win over the
                    // defaults. Pass the host's own environment if you want the guest to
                    // see it.
                    Environment = Map.empty
                }
            // `None` uses the default round-robin thread scheduler; `Some yourChoiceOfSeed`
            // explores thread scheduling intelligently. See ./fuzz-over-thread-scheduler.md.
            PctSeed = None
            // argv passed to the emulated program
            Argv = []
            // Properties to seed `System.AppContext` with before any guest code runs,
            // exactly as a real host does from `runtimeOptions.configProperties` in the
            // app's `runtimeconfig.json`. This is how you set BCL feature switches, e.g.
            // `"System.Diagnostics.Tracing.EventSource.IsSupported", "false"`.
            //
            // To take them from the guest's own runtimeconfig.json, as the CLI does, parse
            // the file and then run the result through `combine`:
            //     RuntimeConfig.parse (File.ReadAllBytes (RuntimeConfig.pathForAssembly dllPath))
            //     |> Result.bind (RuntimeConfig.combine AppContextProperties.empty)
            // Do not skip the second step. `parse` answers only what one file can answer;
            // `combine` merges the dev sidecar (empty, above, if you are not reading one) and
            // applies the checks that need the merged set — chiefly that the config has not
            // claimed a property name the hosting layer owns, which a real host refuses to
            // launch at all. Seeding an unvalidated set would hand the guest a forged
            // `TRUSTED_PLATFORM_ASSEMBLIES` in a configuration CoreCLR would not start.
            //
            // `RuntimeConfig.parse` is pure — reading the file is the host's job, so that a
            // replay never depends on the machine that produced it. It takes the raw bytes
            // because the encoding rules are part of what it reproduces: a real host parses
            // UTF-8 and skips only a UTF-8 BOM, so a UTF-16 config is one it refuses to run.
            AppContext = AppContextProperties.empty
        }

    let terminalState, terminatingThread =
        match Program.run loggerFactory (Some dllPath) peImage hostConfig with
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
        | RunOutcome.FailFast (_, _, message) ->
            let m = message |> Option.defaultValue "<no message>"
            failwith $"Guest called Environment.FailFast: %s{m}"
        | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"
        | RunOutcome.NormalExit (state, thread) -> state, thread
        | RunOutcome.ProcessExit (state, thread) -> state, thread

    let exitCode =
        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | [] -> failwith "expected program to return a value, but it returned void"
        | head :: _ ->
            match head with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
            | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

    // The guest's own writes to stdout/stderr never reach the host's streams during
    // execution: they're accumulated in the kernel's output log, in the order the guest
    // made them, so that a run stays deterministic and replayable. Drain the log at the
    // end if you want to see the output. (PawPrint's *own* logging is a separate matter:
    // the `loggerFactory` above sends it wherever you configured, and `AddConsole` with
    // its default settings will indeed write to the host console during the run.)
    exitCode, terminalState.Kernel.OutputLog
```

`RunOutcome.ProcessExit` is the guest having called `System.Environment.Exit`; like `RunOutcome.NormalExit`, it leaves the exit code on top of the relevant thread's evaluation stack, which is why both cases are handled the same way above.

PawPrint's own tests do this, e.g. in [TestImpureCases.fs](../../WoofWare.PawPrint.Test/TestImpureCases.fs) and [TestPureCases.fs](../../WoofWare.PawPrint.Test/TestPureCases.fs).
We also have [WoofWare.PawPrint.App](../../WoofWare.PawPrint.App/Program.fs), which is a console app that executes a program under PawPrint; it shows how to drain the output log to the host's real standard streams, and how to map each `RunOutcome` onto the exit code a real .NET process would have produced.
