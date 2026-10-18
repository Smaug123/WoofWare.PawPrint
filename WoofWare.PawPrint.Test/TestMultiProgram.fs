namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The driver (`MultiProgram`) running one program: the machine in the program's kernel holds
/// the program's process alone, the driver never idles the one program, and the process is
/// gone from the machine when the program ends.
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
    let ``a program's end leaves the machine without its process, and the driver never idles the one program``
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

        let processId = prepared.State.Kernel.ProcessId

        UnixSystem.processIds prepared.State.Kernel.System
        |> shouldEqual (Set.singleton processId)

        let rec go (driver : MultiProgram) (steps : int) : RunOutcome * UnixSystem<_, _> =
            if not driver.Idle.IsEmpty then
                failwith $"the driver idled its one program at step %d{steps}"

            if
                UnixSystem.processIds driver.Current.State.Kernel.System
                <> Set.singleton processId
            then
                failwith $"the machine held another process at step %d{steps} while its one program ran"

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

        UnixSystem.processIds remaining |> shouldEqual Set.empty
        UnixSystem.checkInvariants remaining |> shouldEqual []
