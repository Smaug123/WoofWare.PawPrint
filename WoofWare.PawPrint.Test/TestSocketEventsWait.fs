namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The park in `SystemNative_WaitForSocketEvents` -- the row the wait exists for, and the one
/// no guest can report on, because a parked guest never returns an exit code.
///
/// `TestSocketEventsWaitReason` covers what `ThreadStatus.BlockedOnSocketEvents` is obliged to
/// answer, constructing the status directly. These tests are the other half: that the handler
/// *reaches* it, and reaches it re-entrantly.
///
/// What is deliberately not pinned here: that the handler leaves `*count` unwritten before
/// parking. Reading a guest local out of a parked frame means depending on Roslyn's slot
/// ordering, and the write would in any case be overwritten by the re-entry that a wake
/// performs -- so the claim is structural rather than observable. The eval-stack assertion
/// below covers the part of it that a re-entry really would corrupt.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventsWait =

    let private assy = typeof<RunResult>.Assembly

    /// Waits on a *duplicated* port descriptor, which is what makes the identity assertion
    /// below bite: `dup` gives fd 4 naming the same open file description as fd 3, so the
    /// status must carry description id 3 and not the descriptor the guest passed. Waiting on
    /// fd 3 directly could not tell the two apart, since a fresh registry hands out
    /// description id 3 to the first port and that is also its descriptor number.
    let private source : string =
        """
using System;
using System.Runtime.InteropServices;

class WaitsOnADuplicatedPort
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_CreateSocketEventPort")]
    static extern unsafe int CreateSocketEventPort(IntPtr* port);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr fd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_WaitForSocketEvents")]
    static extern unsafe int WaitForSocketEvents(IntPtr port, byte* buffer, int* count);

    static unsafe int Main()
    {
        IntPtr port;
        if (CreateSocketEventPort(&port) != 0) return 1;
        if ((long)port != 3) return 2;

        IntPtr alias = Dup(port);
        if ((long)alias != 4) return 3;

        byte* buffer = stackalloc byte[32];
        int count = 1;
        WaitForSocketEvents(alias, buffer, &count);
        return 4;
    }
}
"""

    /// Steps the guest until it deadlocks, returning the state at that point and the
    /// description the driver would report.
    ///
    /// Fails loudly on any other outcome, exit code 4 included: the guest's `return 4` is
    /// unreachable unless the wait returned, so a run that completes has not covered the park.
    let private runToDeadlock () : IlMachineState * string =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "WaitsOnADuplicatedPort.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestSocketEventsWait"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "WaitsOnADuplicatedPort.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        // A bound rather than an unbounded loop: a regression that failed to park would spin
        // here forever instead of failing.
        let maxSteps = 20_000_000L

        let rec loop (prepared : Program.PreparedProgram) (steps : int64) : IlMachineState * string =
            if steps > maxSteps then
                failwith $"guest did not deadlock within %d{maxSteps} steps"

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Deadlocked (prepared, stuck) -> prepared.State, stuck
            | Program.ProgramStepOutcome.Completed outcome ->
                failwith
                    $"guest exited instead of parking in WaitForSocketEvents, so this test covered nothing: %O{outcome}"
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> loop prepared (steps + 1L)
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _) -> loop prepared (steps + 1L)

        loop prepared 0L

    /// One run, shared by every assertion below: reaching the park costs a few million
    /// interpreted steps, and the state is immutable, so there is nothing to isolate.
    let private deadlock = lazy (runToDeadlock ())

    let private parkedThread (state : IlMachineState) : ThreadId * ThreadState =
        state.ThreadState
        |> Map.toList
        |> List.filter (fun (_, ts) ->
            match ts.Status with
            | ThreadStatus.BlockedOnSocketEvents _ -> true
            | _ -> false
        )
        |> function
            | [ one ] -> one
            | other -> failwith $"expected exactly one thread parked on socket events, got %d{List.length other}"

    /// The wait parks the caller rather than answering it, and the port it parks on is the open
    /// file *description* the descriptor names. Waiting through a `dup` is what separates the
    /// two: a handler that stored the descriptor number would say 4 here.
    [<Test>]
    let ``the wait parks the caller on the port's open file description`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        threadState.Status
        |> shouldEqual (ThreadStatus.BlockedOnSocketEvents (OpenFileDescriptionId 3L))

    /// Re-entrant parking, stated as the frame stack: the dispatcher leaves the native frame in
    /// place, so a wake re-enters the handler and it re-reads the call's own arguments. Kills
    /// the mutant that parks and then pops the frame (resume-style parking), under which the
    /// active frame would be the guest's `Main` and the wake would have to write the event
    /// buffer from some other thread's step.
    [<Test>]
    let ``the parked thread still carries the native frame`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        let active = threadState.MethodStates.[threadState.ActiveMethodState]
        active.ExecutingMethod.Name |> shouldEqual "WaitForSocketEvents"

        // And the guest frame that called it is still below, so the native frame was pushed on
        // top rather than replacing anything.
        threadState.MethodStates
        |> Map.exists (fun _ frame -> frame.ExecutingMethod.Name = "Main")
        |> shouldEqual true

    /// Nothing is pushed at park time. The handler returns a PAL error code, so an optimistic
    /// push would be the natural way to write it -- and would be wrong here, because re-entry
    /// runs the handler again and would push a second one.
    [<Test>]
    let ``the wait leaves no optimistic return value on the stack`` () : unit =
        let state, _ = deadlock.Force ()
        let _, threadState = parkedThread state

        let active = threadState.MethodStates.[threadState.ActiveMethodState]
        active.EvaluationStack.Values |> shouldEqual []

    /// The deadlock report has to locate the wait, or a wedged `SocketAsyncEngine` thread is
    /// indistinguishable from any other blocked thread. `TestSocketEventsWaitReason` pins the
    /// rendering against a hand-built position; this pins that a real run reaches it.
    [<Test>]
    let ``the deadlock report names the wait and the port`` () : unit =
        let _, stuck = deadlock.Force ()

        stuck |> shouldContainText "BlockedOnSocketEvents (OpenFileDescriptionId 3L)"
        stuck |> shouldContainText "WaitForSocketEvents"
