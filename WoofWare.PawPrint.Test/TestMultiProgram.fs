namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The driver (`MultiProgram`) running one program: it owns the machine the program's process
/// runs on, never hands that machine over while the one program runs, and ends the process on
/// it when the program ends.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiProgram =

    /// Writes through a direct `SystemNative_Write` P/Invoke rather than `Console`, which would
    /// take minutes of interpretation to set up.
    let private source =
        """
using System;
using System.Runtime.InteropServices;

public static class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write")]
    static extern unsafe int SystemNative_Write(IntPtr fd, byte* buffer, int bufferSize);

    public static unsafe int Main()
    {
        byte[] msg = new byte[] { 104, 105 };
        fixed (byte* p = msg)
        {
            SystemNative_Write((IntPtr)1, p, msg.Length);
        }
        return 3;
    }
}
"""

    [<Test>]
    let ``a program's end leaves the driver's machine without its process, and the machine is never handed over before``
        ()
        : unit
        =
        let sourceName = "MultiProgramOne.cs"
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestMultiProgram"
        use peImage = new MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some sourceName)
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        let machine = prepared.Driver.Machine
        let processId = UnixSystem.processId prepared.State.Kernel.System
        SimulatedMachine.processIds machine |> shouldEqual (Set.singleton processId)

        let rec go (driver : MultiProgram) (steps : int) : RunOutcome * SimulatedMachine<_, _> =
            // A program of one never hands the machine over: a handover would cost a focus and
            // an unfocus on every tick.
            if not (obj.ReferenceEquals (driver.Machine, machine)) then
                failwith $"the driver's machine changed at step %d{steps} while its one program ran"

            match MultiProgram.step loggerFactory logger driver with
            | MultiStepOutcome.Stepped (driver,
                                        pid,
                                        (ProgramEvent.InstructionStepped _ | ProgramEvent.WorkerTerminated _)) ->
                pid |> shouldEqual processId
                go driver (steps + 1)
            | MultiStepOutcome.Finished ([ pid, RunEnd.Ended outcome ], remaining) ->
                pid |> shouldEqual processId
                outcome, remaining
            | MultiStepOutcome.Finished (ends, _) -> failwith $"expected one program's end, got %A{ends}"
            | MultiStepOutcome.Deadlocked (_, stuck) -> failwith $"deadlocked: %A{stuck}"
            | MultiStepOutcome.Stepped (_, _, ProgramEvent.PhaseAdvanced) ->
                failwith "a startup call returned after Main was installed"
            | MultiStepOutcome.Stepped (_, _, ProgramEvent.Ended _) ->
                failwith "the one program ended and the driver ran on"

        let outcome, remaining = go prepared.Driver 0

        match outcome with
        | RunOutcome.NormalExit (state, _, ProcessTermination.Exited code) ->
            ExitStatus.waitpidExitCode code |> shouldEqual 3

            OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
            |> Seq.toList
            |> shouldEqual [ 104uy ; 105uy ]
        | other -> failwith $"expected a normal exit, got %O{other}"

        SimulatedMachine.processIds remaining |> shouldEqual Set.empty
        SimulatedMachine.checkInvariants remaining |> shouldEqual []
