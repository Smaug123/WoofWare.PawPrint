namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The stack-shape analysis of a body minted by `Reflection.Emit`, whose token effects come from
/// its `DynamicScope` rather than from metadata. Dynamic code has to be enabled for the guest to
/// emit at all, so these cannot be pure cases; each guest passes on real .NET (exit 0, measured
/// 2026-09-11).
[<TestFixture>]
[<Category("Guest")>]
[<Explicit>]
module TestStackShapeDynamicScope =
    let private assy = typeof<RunResult>.Assembly

    let private run (sourceName : string) : unit =
        let source = Assembly.getEmbeddedResourceAsString sourceName assy
        let image = Roslyn.compile [ source ]

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let terminalState =
            match BoundedRun.run loggerFactory sourceName None peImage hostConfig with
            | RunOutcome.NormalExit (state, _) -> state
            | RunOutcome.ProcessExit (state, _) -> state
            | RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
            | RunOutcome.Aborted (_, _, fatal) ->
                let message = fatal.Message |> Option.defaultValue "<no message>"
                failwith $"Guest aborted (%O{fatal.Code}): %s{message}"
            | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"

        terminalState.LatchedExitCode |> shouldEqual 0

    [<Test>]
    let ``a call to a dynamic method not yet minted takes its arity from the DynamicMethod object`` () : unit =
        // The callee has never run when the caller first executes, so its signature is read
        // from `_parameterTypes` and `_returnType` rather than from a minted body.
        run "DynamicMethodCallsUnminted.cs"

    [<Test>]
    let ``a scope entry swapped for a reflected method after the mint does not stop the body`` () : unit =
        // The decoder refuses a `RuntimeMethodHandle` in method position when the body is minted,
        // but a guest can put one there afterwards. PawPrint refuses the call when it executes; a
        // call that never executes must not be refused ahead of it.
        run "DynamicScopeSwappedAfterMint.cs"
