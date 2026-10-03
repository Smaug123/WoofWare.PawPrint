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
/// default says for every signal the table leaves at its default, and must not
/// end that way for any signal the table catches or ignores. Sending one the
/// table catches or ignores a second time must end that way exactly when the
/// table says the runtime's handler restores the default.
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

    /// What the test claims for one signal, decided before the guest runs,
    /// because the guest must be told whether to wait for its own death.
    [<RequireQualifiedAccess>]
    type private Claim =
        /// This test host ignores the signal, and the oracle's child inherits
        /// that, so the row says nothing about the runtime.
        | Inherited
        /// The table says the runtime overrides the terminating default, for
        /// every instance of the signal.
        | Overridden
        /// The table says the runtime overrides the terminating default for the
        /// first instance, and then restores it.
        | OverriddenOnce
        /// The kernel's default terminates, and the table does not override it.
        | Dies
        /// The kernel's default does not terminate.
        | Survives

    // The guest sends the signal as many times as its second argument says.
    // With "await-death", it never returns after that: a run that expects
    // death must observe it, because the guest cannot observe
    // that it has survived. CoreCLR's handlers for SIGINT, SIGQUIT and SIGTERM
    // restore the default and send the signal again, and the handler runs with
    // the signal masked on the thread that sent it, so on Darwin the second,
    // fatal copy is pending on another thread when kill returns. Nothing
    // orders that thread's next run before Main's return, and sigpending
    // reports only the calling thread's signals. Only runs that do not expect
    // death return after kill, and a race there can only disguise a death as
    // survival. A `Survives` row's default cannot kill the process. A run of
    // an overridden row that expects survival would miss a death only if the
    // runtime killed the process with a second copy of the signal sent from
    // its handler, which is not how any signal in the table ends.
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
        int signo = int.Parse(args[0]);
        for (int i = int.Parse(args[1]); i > 0; i--)
        {
            if (Kill(Environment.ProcessId, signo) != 0) return 1;
        }
        switch (args[2])
        {
            case "await-death": Thread.Sleep(Timeout.Infinite); return 2;
            case "return": return 42;
            default: return 3;
        }
    }
}
"""

    [<Test>]
    let ``the host runtime departs from the kernel default exactly where the table says`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            let image = Roslyn.compile [ guest ]

            let signalOf (signo : int) : Signal =
                match Signal.ofRawSignoUnder numbering signo with
                | ValueSome signal -> signal
                | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

            let startup : Map<Signal, SignalDisposition<NativeSignalHandler>> =
                UnixSystem.initial (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)
                |> UnixBootImage.boot
                |> StartupSignalDispositions.install "test" numbering Set.empty
                |> KernelSignals.dispositions

            let startupDisposition (signal : Signal) : SignalDisposition<NativeSignalHandler> =
                Map.tryFind signal startup |> Option.defaultValue SignalDisposition.Default

            // A stop signal would stop the child until the oracle's timeout
            // kills it, so those are left out; the model refuses them anyway.
            let signos =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.filter (fun signo ->
                    Signal.defaultDispositionUnder numbering (signalOf signo)
                    <> DefaultDisposition.Stop
                )

            let claimOf (signo : int) : Claim =
                let signal = signalOf signo

                let overridden = startupDisposition signal <> SignalDisposition.Default

                let restored =
                    match startupDisposition signal with
                    | SignalDisposition.Catch {
                                                  Handler = NativeSignalHandler.CoreClrPalFault _
                                              } -> true
                    | _ -> false

                let terminatesByDefault =
                    Signal.defaultDispositionUnder numbering signal = DefaultDisposition.Terminate

                // A row the launcher ignores says nothing about the runtime
                // unless the runtime's own disposition decides it anyway.
                if hostIgnores signo && terminatesByDefault && not overridden then
                    Claim.Inherited
                // Over an inherited ignore, the fault handler aborts the process
                // at the first signal instead of restoring the default (see
                // `NativeSignalHandler.CoreClrPalFault`), so only the claim every
                // overridden row makes is checked: that the default is not what
                // happens.
                elif overridden && restored && not (hostIgnores signo) then
                    Claim.OverriddenOnce
                elif overridden then
                    Claim.Overridden
                elif terminatesByDefault then
                    Claim.Dies
                else
                    Claim.Survives

            let claims = signos |> List.map (fun signo -> signo, claimOf signo)

            // Each run is a signal, how many times the guest sends it, and
            // whether the guest then waits for its death or returns.
            let runsOf (signo : int, claim : Claim) : (int * int * string) list =
                match claim with
                | Claim.Inherited -> []
                | Claim.Dies -> [ signo, 1, "await-death" ]
                | Claim.Survives -> [ signo, 1, "return" ]
                | Claim.Overridden -> [ signo, 1, "return" ; signo, 2, "return" ]
                | Claim.OverriddenOnce -> [ signo, 1, "return" ; signo, 2, "await-death" ]

            // An exception from the oracle (in a run that expects death, most
            // likely its timeout, because the runtime survived) is kept against
            // its run rather than escaping the loop unattributed.
            let observed = ConcurrentDictionary<int * int, Result<RealRuntimeResult, exn>> ()

            Parallel.ForEach (
                List.collect runsOf claims,
                ParallelOptions (MaxDegreeOfParallelism = 8),
                fun (signo : int, sends : int, mode : string) ->
                    observed.[(signo, sends)] <-
                        try
                            Ok (
                                RealRuntime.executeWithRealRuntime
                                    [| string<int> signo ; string<int> sends ; mode |]
                                    image
                            )
                        with e ->
                            Error e
            )
            |> ignore<ParallelLoopResult>

            let describeRun (signo : int) (sends : int) : string =
                let times = if sends = 1 then "sent once" else $"sent %d{sends} times"
                $"signo %d{signo} (%O{signalOf signo}), %s{times}"

            let exact (signo : int) (sends : int) (expected : RealRuntimeResult) : string option =
                match observed.[(signo, sends)] with
                | Ok result when result = expected -> None
                | Ok result ->
                    Some $"%s{describeRun signo sends}: expected %O{expected}, the real runtime gave %O{result}"
                | Error e ->
                    Some
                        $"%s{describeRun signo sends}: expected %O{expected}, the real runtime could not be observed: %s{e.Message}"

            // What the runtime's disposition does instead of the default is not
            // one answer: the process survives most of these, but on x86-64
            // Linux SIGTRAP kills it with SIGILL. The table claims only that the
            // default is not what happens.
            let notDiedOfIt (signo : int) (sends : int) : string option =
                match observed.[(signo, sends)] with
                | Ok result when result = RealRuntimeResult.NormalExit (128 + signo) ->
                    Some
                        $"%s{describeRun signo sends}: the table says the runtime overrides the default, but the real runtime died of it (%O{result})"
                | Ok _ -> None
                | Error e -> Some $"%s{describeRun signo sends}: the real runtime could not be observed: %s{e.Message}"

            let mismatchesOf (signo : int, claim : Claim) : string list =
                let diedOfIt = RealRuntimeResult.NormalExit (128 + signo)

                match claim with
                | Claim.Inherited ->
                    printfn
                        $"signo %d{signo} (%O{signalOf signo}): not checked, because this test host ignores it and the oracle's child inherits that."

                    []
                | Claim.Overridden -> List.choose id [ notDiedOfIt signo 1 ; notDiedOfIt signo 2 ]
                | Claim.OverriddenOnce -> List.choose id [ notDiedOfIt signo 1 ; exact signo 2 diedOfIt ]
                | Claim.Dies -> Option.toList (exact signo 1 diedOfIt)
                | Claim.Survives -> Option.toList (exact signo 1 (RealRuntimeResult.NormalExit 42))

            let mismatches = claims |> List.collect mismatchesOf

            if not mismatches.IsEmpty then
                failwith (
                    $"The %O{numbering} column of StartupSignalDispositions disagrees with the host runtime:\n"
                    + String.concat "\n" mismatches
                )
        )
