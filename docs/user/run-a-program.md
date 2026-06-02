# How to run a program under PawPrint

PawPrint is intended to be used in tests, so is primarily a library.

Use whatever `dotnet build` command you'd usually use to compile an executable DLL, say `/tmp/MyCoolDll.dll`.
For the most predictable results, I'd probably `dotnet publish --self-contained` this; otherwise you may find [WoofWare.DotnetRuntimeLocator](https://github.com/Smaug123/WoofWare.DotnetRuntimeLocator) fails to locate the correct .NET runtime (although it *should* work).

```fsharp
let pathToDll = "/tmp/MyCoolDll.dll"

use loggerFactory =
    Microsoft.Extensions.Logging.LoggerFactory.Create(fun builder -> builder.AddConsole ());

use peImage = new System.IO.FileStream (dllPath, System.IO.FileMode.Open, System.IO.FileAccess.Read)

use dotnetRuntimes =
    WoofWare.DotnetRuntimeLocator.DotnetRuntime.SelectForDll dllPath
    |> System.Collections.Immutable.ImmutableDictionary.CreateRange

// When PawPrint requires a native call, e.g. a result from System.Environment,
// just pass through to the host.
// This actually isn't a pure pass-through: System.Environment.FailFast, for example,
// is trapped.
let nativeImpls = WoofWare.PawPrint.NativeImpls.PassThru ()

// I recommend running with at least the default environment variables,
// because that way you get invariant globalization turned on.
let environmentVariables = WoofWare.PawPrint.EmulatedKernel.defaultEnvironment

let terminalState, terminatingThread =
    match
        WoofWare.PawPrint.Program.run
            loggerFactory
            (Some pathToDll)
            peImage
            dotnetRuntimes
            nativeImpls
            environmentVariables
            None // use a default thread scheduler; or `Some yourChoiceOfSeed` to explore thread scheduling intelligently
            [] // argv passed to emulated program
    with
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
        | EvalStackValue.Int32 i -> i
        | ret -> failwith $"expected program to return an int, but it returned %O{ret}"
```

PawPrint's own tests do this, e.g. in [TestImpureCases.fs](../../WoofWare.PawPrint.Test/TestImpureCases.fs) and [TestPureCases.fs](../../WoofWare.PawPrint.Test/TestPureCases.fs).
We also have [WoofWare.PawPrint.App](../../WoofWare.PawPrint.App/Program.fs), which is a console app that executes a program under PawPrint.