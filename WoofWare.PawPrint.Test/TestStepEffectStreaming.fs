namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins the contract a *streaming* driver depends on: every guest write is reported to the
/// driver, as it happens, by the step that performed it.
///
/// `SystemNative_Write` does two things with each guest write — appends it to
/// `EmulatedKernel.OutputLog` and reports it as `StepEffect.WroteToFd` — and the effect is
/// what lets a driver write to a real stream while the guest is still running.
/// `Program.stepPrepared` used to discard it (`ExecutionResult.Stepped (state, whatWeDid, _)`),
/// so the only way to see guest output was to wait for a `RunOutcome` and drain the log.
///
/// The difference is not cosmetic: a run that never yields a `RunOutcome` has no end-of-run
/// drain to reach. A guest that livelocks, or is killed from outside, or that this interpreter
/// reports as `Deadlocked`, loses everything it printed — which is exactly when the output is
/// most wanted. `WoofWare.PawPrint.App` therefore consumes these effects, and the two tests
/// here pin the two halves it relies on.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStepEffectStreaming =

    let private assy = typeof<RunResult>.Assembly

    /// Guests here write via a direct `SystemNative_Write` P/Invoke rather than
    /// `Console.WriteLine`. That is not merely faster: `Console.Out` drags in the whole
    /// TextWriter/Encoding stack, which takes minutes of interpretation and would make these
    /// tests useless. The write path under test is identical either way — `Console` bottoms
    /// out in this same call.
    let private writeDecl =
        """
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    static unsafe int Write(int fd, byte[] msg)
    {
        fixed (byte* p = msg)
        {
            return SystemNative_Write((IntPtr)fd, p, msg.Length);
        }
    }
"""

    /// Every `WroteToFd` a run reported, in the order the steps produced them.
    type private Streamed =
        {
            Effects : (FileDescriptorRole * byte array) list
            /// Log entries already present when `prepare` returned, i.e. writes performed by a
            /// static initialiser before the driver's stepping loop begins. These never pass
            /// through a `StepEffect`, which is why a streaming driver must also drain the tail
            /// of the log; asserted to be zero here so the comparison below is exhaustive.
            BeforeMain : int
            Terminal : IlMachineState
        }

    let private entriesOf (state : IlMachineState) : (FileDescriptorRole * byte array) list =
        state.Kernel.OutputLog
        |> Seq.map (fun entry -> entry.Role, entry.Bytes |> Seq.toArray)
        |> List.ofSeq

    /// Drive the guest one step at a time, collecting `WroteToFd` effects exactly as
    /// `WoofWare.PawPrint.App` does. `stopAtDeadlock` decides whether a deadlocked guest is the
    /// expected result or a failure.
    let private stream (sourceName : string) (source : string) (expectDeadlock : bool) : Streamed =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestStepEffectStreaming"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes) with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        let beforeMain = prepared.State.Kernel.OutputLog.Length

        let rec loop
            (prepared : Program.PreparedProgram)
            (acc : (FileDescriptorRole * byte array) list)
            : (FileDescriptorRole * byte array) list * IlMachineState
            =
            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                if expectDeadlock then
                    failwith $"expected guest to deadlock, but it completed: %O{outcome}"

                match outcome with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim 0) :: _ -> List.rev acc, terminalState
                    | other -> failwith $"guest did not return exit code 0: %O{other}"
                | other -> failwith $"guest did not exit normally: %O{other}"
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) ->
                if not expectDeadlock then
                    failwith $"guest deadlocked: %s{stuck}"

                List.rev acc, prepared.State
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> loop prepared acc
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, effect) ->
                let acc =
                    match effect with
                    | StepEffect.WroteToFd (role, bytes) -> (role, bytes |> Seq.toArray) :: acc
                    | StepEffect.NoEffect -> acc

                loop prepared acc

        let effects, terminal = loop prepared []

        {
            Effects = effects
            BeforeMain = beforeMain
            Terminal = terminal
        }

    /// The driver sees every write, once each, with the right bytes and the right stream — the
    /// effects reconstruct the `OutputLog` exactly.
    ///
    /// Comparing against the log rather than against a hard-coded list is what makes this
    /// load-bearing in both directions: an effect that went missing shortens the left side, and
    /// one reported twice (or with the wrong role or bytes) breaks the match, without the test
    /// having to enumerate what the BCL start-up path might legitimately write.
    [<Test>]
    let ``every guest write is reported to the driver as it happens`` () =
        let source =
            """
using System;
using System.Runtime.InteropServices;

class Program
{"""
            + writeDecl
            + """
    static int Main(string[] args)
    {
        // Interleave the two streams: cross-stream ordering is part of the contract, and a
        // driver that streams must preserve it.
        if (Write(1, new byte[] { 0x6F, 0x6E, 0x65, 0x0A }) != 4) return 1;   // "one\n"
        if (Write(2, new byte[] { 0x74, 0x77, 0x6F, 0x0A }) != 4) return 2;   // "two\n"
        if (Write(1, new byte[] { 0x74, 0x68, 0x72, 0x0A }) != 4) return 3;   // "thr\n"
        return 0;
    }
}
"""

        let result = stream "StepEffectWrites.cs" source false

        // Nothing wrote before Main, so the effects should account for the whole log. If a
        // future start-up path starts printing, this fires rather than silently weakening the
        // comparison below.
        result.BeforeMain |> shouldEqual 0

        result.Effects |> shouldEqual (entriesOf result.Terminal)

        // ...and the log really does contain the three writes, so the equality above is not
        // two empty lists agreeing with each other.
        result.Effects
        |> shouldEqual
            [
                FileDescriptorRole.StandardOutput, [| 0x6Fuy ; 0x6Euy ; 0x65uy ; 0x0Auy |]
                FileDescriptorRole.StandardError, [| 0x74uy ; 0x77uy ; 0x6Fuy ; 0x0Auy |]
                FileDescriptorRole.StandardOutput, [| 0x74uy ; 0x68uy ; 0x72uy ; 0x0Auy |]
            ]

    /// A guest that prints and then wedges still delivers what it printed.
    ///
    /// This is the case the end-of-run drain could never serve: `Deadlocked` is not a
    /// `RunOutcome`, and `Program.pumpPrepared` raises on it rather than returning, so a driver
    /// built on `Program.run` had nothing to drain. Both halves are asserted — the effects the
    /// driver already streamed, and the state `Deadlocked` carries, which is what lets a driver
    /// flush any tail it had not yet written.
    [<Test>]
    let ``a deadlocked guest still delivers what it wrote`` () =
        let source =
            """
using System;
using System.Runtime.InteropServices;
using System.Threading;

class Program
{"""
            + writeDecl
            + """
    static int Main(string[] args)
    {
        if (Write(1, new byte[] { 0x62, 0x65, 0x66, 0x0A }) != 4) return 1;   // "bef\n"

        // Nothing ever sets this, and no other thread exists, so every thread is blocked and
        // the entry thread has not terminated: the interpreter reports Deadlocked.
        new ManualResetEventSlim(false).Wait();

        return 0;
    }
}
"""

        let result = stream "StepEffectDeadlock.cs" source true

        result.BeforeMain |> shouldEqual 0

        result.Effects
        |> shouldEqual [ FileDescriptorRole.StandardOutput, [| 0x62uy ; 0x65uy ; 0x66uy ; 0x0Auy |] ]

        // The outcome carries the state, so a driver can also recover the output from the log.
        OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput result.Terminal.Kernel.OutputLog
        |> Seq.toArray
        |> shouldEqual [| 0x62uy ; 0x65uy ; 0x66uy ; 0x0Auy |]
