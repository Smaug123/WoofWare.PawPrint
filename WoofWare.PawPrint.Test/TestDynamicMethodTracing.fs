namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// <summary>
/// That executing a <c>Reflection.Emit</c> method survives having trace logging turned on.
/// </summary>
/// <remarks>
/// <para>
/// This exists because the ordinary end-to-end fixtures cannot see the failure it guards against.
/// <c>LoggerFactory.makeTest</c> runs at <c>Information</c>, so the per-instruction trace block in
/// <c>AbstractMachine.executeOneStepInitialised</c> is never entered by
/// <c>sourcesImpure/DynamicMethodInvoke.cs</c> — and that block reads the executing method's
/// declaring type, which a dynamic method does not have. The result was a crash that appeared only
/// under <c>PAWPRINT_LOG_LEVEL=Trace</c>: precisely the configuration someone turns on to
/// investigate a dynamic method, and invisible to a suite of 3000 tests.
/// </para>
/// <para>
/// So the assertion is not about the log's *contents* — nothing here reads a line back. It is that
/// the guest still reaches its own exit code with every level enabled. A fixture that captured
/// output would be slower and would test less.
/// </para>
/// </remarks>
[<TestFixture>]
module TestDynamicMethodTracing =

    let private assy = typeof<RunResult>.Assembly

    /// Claims to be enabled at every level, including <c>Trace</c>, and discards everything. The
    /// "enabled" half is the whole point; the discarding half keeps the test from being dominated
    /// by formatting one line per interpreted instruction.
    let private traceEnabledFactory : ILoggerFactory =
        { new ILoggerFactory with
            member _.CreateLogger (_categoryName : string) : ILogger =
                { new ILogger with
                    member _.BeginScope _state =
                        { new IDisposable with
                            member _.Dispose () = ()
                        }

                    member _.IsEnabled (_ : LogLevel) : bool = true

                    member _.Log (_logLevel, _eventId, _state, _ex, _formatter) = ()
                }

            member _.AddProvider _provider = ()
            member _.Dispose () = ()
        }

    [<Test>]
    let ``a dynamic method executes with trace logging enabled`` () : unit =
        // The same guest the end-to-end case runs, so this cannot drift away from what it covers.
        let source = Assembly.getEmbeddedResourceAsString "DynamicMethodInvoke.cs" assy

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

        let terminalState =
            match BoundedRun.run traceEnabledFactory "DynamicMethodInvoke.cs (traced)" None peImage hostConfig with
            | RunOutcome.NormalExit (state, _) -> state
            | RunOutcome.ProcessExit (state, _) -> state
            | RunOutcome.GuestUnhandledException (_, _, exn) ->
                failwith $"Guest threw unhandled exception: %O{exn.ExceptionObject}"
            | RunOutcome.Aborted (_, _, fatal) ->
                let message = fatal.Message |> Option.defaultValue "<no message>"
                failwith $"Guest aborted (%O{fatal.Code}): %s{message}"
            | RunOutcome.SignalTerminated (_, signal) -> failwith $"Guest was terminated by POSIX signal %O{signal}"

        terminalState.LatchedExitCode |> shouldEqual 0
