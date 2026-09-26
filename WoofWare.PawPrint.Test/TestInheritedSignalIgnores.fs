namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `KernelConfig.InheritedSignalIgnores` against the real runtime started the
/// same way: a guest whose launcher left SIGHUP and SIGUSR2 ignored survives
/// sending itself either, and a `PosixSignalRegistration` for the ignored
/// SIGHUP never runs. Without the ignores, the same guest dies of SIGUSR2 on
/// both, which is what shows the launcher's ignores reached each side.
///
/// Runs on the host's own flavour, because the real runtime can only speak
/// for the host: SIGUSR2's number differs between the two.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
// Runs guests under the interpreter; see AGENTS.md for why that makes it
// `Explicit`, and CI selects it by category.
[<Category("Guest")>]
[<Explicit>]
module TestInheritedSignalIgnores =

    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        int pid = Environment.ProcessId;
        int usr2 = int.Parse(args[0]);

        // Nothing is registered for SIGUSR2, whose default terminates.
        if (Kill(pid, usr2) != 0) return 1;

        int invocations = 0;

        using (PosixSignalRegistration.Create(PosixSignal.SIGHUP, _ => Interlocked.Increment(ref invocations)))
        {
            if (Kill(pid, 1) != 0) return 2;

            // Long enough for the handler to have run, had SIGHUP reached it.
            Thread.Sleep(TimeSpan.FromSeconds(1));
        }

        if (Volatile.Read(ref invocations) != 0) return 3;

        return 0;
    }
}
"""

    let private runUnderPawPrint
        (platform : SimulatedUnixPlatform)
        (ignored : Set<Signal>)
        (usr2 : int)
        (image : byte array)
        : RunOutcome
        =
        let description = $"inherited ignores %A{ignored}"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "case", description ]

        use _loggerFactoryResource = loggerFactory
        let dotnetRuntimes = FrameworkUnderTest.runtimeDirs ()
        use peImage = new MemoryStream (image)

        BoundedRun.run
            loggerFactory
            description
            None
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                UnixPlatform = platform
                                InheritedSignalIgnores = ignored
                            }
                        Argv = [ string<int> usr2 ]
                    }
            }

    [<Test>]
    let ``a guest started with signals ignored survives them on both runtimes`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let numbering = SimulatedUnixPlatform.signalNumbering platform
            let usr2 = Signal.toRawSignoUnder numbering Signal.SIGUSR2
            let image = Roslyn.compile [ guest ]

            match runUnderPawPrint platform (Set.ofList [ Signal.SIGHUP ; Signal.SIGUSR2 ]) usr2 image with
            | RunOutcome.NormalExit (state, _) -> state.LatchedExitCode |> shouldEqual 0
            | other -> failwith $"PawPrint: expected a clean exit, got %O{other}"

            RealRuntime.executeWithInheritedIgnores [ 1 ; usr2 ] [| string<int> usr2 |] image
            |> shouldEqual (RealRuntimeResult.NormalExit 0)
        )

    [<Test>]
    let ``without the ignores the same guest dies of SIGUSR2 on both runtimes`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let numbering = SimulatedUnixPlatform.signalNumbering platform
            let usr2 = Signal.toRawSignoUnder numbering Signal.SIGUSR2
            let image = Roslyn.compile [ guest ]

            match runUnderPawPrint platform Set.empty usr2 image with
            | RunOutcome.SignalTerminated (_, signal, coreDumped) ->
                signal |> shouldEqual Signal.SIGUSR2
                coreDumped |> shouldEqual false
            | other -> failwith $"PawPrint: expected death by SIGUSR2, got %O{other}"

            RealRuntime.executeWithRealRuntime [| string<int> usr2 |] image
            |> shouldEqual (RealRuntimeResult.NormalExit (128 + usr2))
        )
