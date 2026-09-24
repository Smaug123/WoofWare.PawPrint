namespace WoofWare.PawPrint.Test

open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Threading.Tasks
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `StartupSignalDispositions` against the real runtime this test host runs:
/// a program that sends itself each signal in turn must survive exactly the
/// signals the table and the kernel's defaults say it survives, and die of
/// every other.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStartupSignalDispositions =

    [<DllImport("libc", EntryPoint = "sigaction", SetLastError = true)>]
    extern int private hostSigaction(int signo, nativeint act, nativeint oldAct)

    /// Whether this test host ignores `signo`. An ignored disposition survives
    /// `execve`, so the oracle's child starts with it too, and a launcher that
    /// ignores a signal (`nohup`, or a shell running a job in the background)
    /// would otherwise read as a runtime that survives it. `struct sigaction`
    /// begins with the handler on both flavours, and `SIG_IGN` is 1 on both.
    let private hostIgnores (signo : int) : bool =
        let buffer = Marshal.AllocHGlobal 1024

        try
            for offset in 0..8..1016 do
                Marshal.WriteInt64 (buffer, offset, 0L)

            // A failure (glibc's reserved 32 and 33) reads as "not ignored".
            hostSigaction (signo, 0n, buffer) = 0 && Marshal.ReadInt64 buffer = 1L
        finally
            Marshal.FreeHGlobal buffer

    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libc", EntryPoint = "kill", SetLastError = true)]
    static extern int Kill(int pid, int sig);

    static int Main(string[] args)
    {
        if (Kill(Environment.ProcessId, int.Parse(args[0])) != 0) return 1;
        return 42;
    }
}
"""

    [<Test>]
    let ``the host runtime survives exactly the signals the table says it does`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            let image = Roslyn.compile [ guest ]

            // A stop signal would stop the child until the oracle's timeout
            // kills it, so those are left out; the model refuses them anyway.
            let signos =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.filter (fun signo ->
                    match Signal.ofRawSignoUnder numbering signo with
                    | ValueSome signal -> Signal.defaultDispositionUnder numbering signal <> DefaultDisposition.Stop
                    | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"
                )

            let observed = ConcurrentDictionary<int, RealRuntimeResult> ()

            Parallel.ForEach (
                signos,
                ParallelOptions (MaxDegreeOfParallelism = 8),
                fun (signo : int) ->
                    observed.[signo] <- RealRuntime.executeWithRealRuntime [| string<int> signo |] image
            )
            |> ignore<ParallelLoopResult>

            let mismatches =
                [
                    for signo in signos do
                        let signal =
                            match Signal.ofRawSignoUnder numbering signo with
                            | ValueSome signal -> signal
                            | ValueNone -> failwith "unreachable: filtered above"

                        let survives =
                            StartupSignalDispositions.survivesDespiteTerminatingDefault numbering signal
                            || Signal.defaultDispositionUnder numbering signal <> DefaultDisposition.Terminate

                        let expected =
                            if survives then
                                RealRuntimeResult.NormalExit 42
                            else
                                RealRuntimeResult.NormalExit (128 + signo)

                        // A row the launcher ignores says nothing about the
                        // runtime unless the runtime survives it anyway.
                        let inherited = hostIgnores signo && not survives

                        if inherited then
                            printfn
                                $"signo %d{signo} (%O{signal}): not checked, because this test host ignores it and the oracle's child inherits that."
                        elif observed.[signo] <> expected then
                            yield
                                $"signo %d{signo} (%O{signal}): expected %O{expected}, the real runtime gave %O{observed.[signo]}"
                ]

            if not mismatches.IsEmpty then
                failwith (
                    $"The %O{numbering} column of StartupSignalDispositions disagrees with the host runtime:\n"
                    + String.concat "\n" mismatches
                )
        )
