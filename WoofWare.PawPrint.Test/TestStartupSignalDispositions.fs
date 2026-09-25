namespace WoofWare.PawPrint.Test

open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Threading.Tasks
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `StartupSignalDispositions` against the real runtime this test host runs:
/// a program that sends itself each signal in turn must end as the kernel's
/// default says for every signal the table does not name, and must not end
/// that way for any signal it does.
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
    let ``the host runtime departs from the kernel default exactly where the table says`` () : unit =
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

                        let diedOfIt = RealRuntimeResult.NormalExit (128 + signo)
                        let survived = RealRuntimeResult.NormalExit 42

                        let overridden =
                            StartupSignalDispositions.overridesTerminatingDefault numbering signal

                        let terminatesByDefault =
                            Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Terminate

                        // A row the launcher ignores says nothing about the
                        // runtime unless the runtime's own disposition decides
                        // it anyway.
                        let inherited = hostIgnores signo && terminatesByDefault && not overridden

                        if inherited then
                            printfn
                                $"signo %d{signo} (%O{signal}): not checked, because this test host ignores it and the oracle's child inherits that."
                        elif overridden then
                            // What the runtime's disposition does instead is
                            // not one answer: the process survives most of
                            // these, but on x86-64 Linux SIGTRAP kills it with
                            // SIGILL. The table claims only that the default
                            // is not what happens.
                            if observed.[signo] = diedOfIt then
                                yield
                                    $"signo %d{signo} (%O{signal}): the table says the runtime overrides the default, but the real runtime died of it (%O{diedOfIt})"
                        else
                            let expected = if terminatesByDefault then diedOfIt else survived

                            if observed.[signo] <> expected then
                                yield
                                    $"signo %d{signo} (%O{signal}): expected %O{expected}, the real runtime gave %O{observed.[signo]}"
                ]

            if not mismatches.IsEmpty then
                failwith (
                    $"The %O{numbering} column of StartupSignalDispositions disagrees with the host runtime:\n"
                    + String.concat "\n" mismatches
                )
        )
