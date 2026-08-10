namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins what `calli` reports to the scheduler when the call does not commit.
///
/// `callMethod` can decline to commit: an intrinsic callee that needs a class initialiser run
/// pushes that cctor frame and leaves the calling instruction's PC unadvanced so the
/// instruction re-executes. `executeCalli` restores its function pointer for that retry, and
/// must also *report* the suspension — the two are the same fact and have to agree.
///
/// `Scheduler.onStepOutcome` treats `Executed` as forward progress and wakes every thread
/// parked `BlockedOnClassInit` on this one, whereas `SuspendedForClassInit` leaves them parked
/// because the class init has *not* finished. Reporting `Executed` here would wake a waiter
/// while the cctor is still running; it would take a step and re-block.
///
/// That is a fidelity bug rather than a correctness one, and the distinction is worth keeping
/// straight: the woken thread re-checks its blocker, so the result is unaffected and the
/// schedule stays deterministic — `Scheduler.onStepOutcome`'s own doc calls the speculative
/// wake "correct but wasteful". The reason to pin it is that the outcome is derived from the
/// same value as the function-pointer restore, which *is* load-bearing, so a regression in one
/// is a regression in the other. Note the sibling call ops still report `Executed`
/// unconditionally in this situation.
///
/// Asserted by stepping the guest and looking only at steps whose executing instruction is
/// itself a `calli`, so an unrelated class init elsewhere in start-up cannot satisfy it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCalliSuspensionOutcome =

    let private assy = typeof<RunResult>.Assembly

    /// The IL op the thread is about to execute, when that is knowable. `None` for frames whose
    /// body is not IL (native/internal-call handlers), which we simply do not classify.
    let private currentIlOp (thread : ThreadId) (state : IlMachineState) : IlOp option =
        match state.ThreadState |> Map.tryFind thread with
        | None -> None
        | Some threadState ->
            let methodState = threadState.MethodState

            match MethodBody.tryIl methodState.ExecutingMethod.Body with
            | Some instructions ->
                match instructions.Locations.TryGetValue methodState.IlOpIndex with
                | true, op -> Some op
                | false, _ -> None
            | None -> None

    let private isCalli (op : IlOp) : bool =
        match op with
        | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, _) -> true
        | _ -> false

    /// Steps the guest to completion, returning the scheduler-visible outcome of exactly those
    /// steps whose executing instruction was a `calli`, together with the guest's exit code.
    ///
    /// Restricting to `calli` steps matters: class initialisation happens all over start-up, so a
    /// test that looked at every step could be satisfied by something entirely unrelated to the
    /// instruction under test. The exit code matters too — without it, a run that reported the
    /// right outcomes while corrupting the guest's state would still pass.
    let private calliOutcomes (sourceName : string) (source : string) : WhatWeDid list * int =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestCalliSuspensionOutcome"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match Program.prepare loggerFactory (Some sourceName) peImage (HostConfig.Default dotnetRuntimes) with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        // `stepPrepared` reports which thread it ran, so attribute the outcome to *that* thread's
        // pre-step instruction. Reading a fixed thread's instruction instead would mis-attribute
        // as soon as the scheduler interleaves, which these single-threaded guests happen not to
        // do — an accident not worth depending on.
        let rec loop (prepared : Program.PreparedProgram) (acc : WhatWeDid list) : WhatWeDid list * int =
            let stateBefore = prepared.State

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                match outcome with
                | RunOutcome.NormalExit (terminalState, terminatingThread)
                | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> List.rev acc, exitCode
                    | other -> failwith $"guest did not return an int exit code: %O{other}"
                | other -> failwith $"guest did not exit normally: %O{other}"
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p acc
            | Program.ProgramStepOutcome.InstructionStepped (p, ran, whatWeDid, _) ->
                let acc =
                    match currentIlOp ran stateBefore with
                    | Some op when isCalli op -> whatWeDid :: acc
                    | _ -> acc

                loop p acc

        let observed, exitCode = loop prepared []

        // Guard the guard: with no `calli` step observed, every assertion below would hold
        // vacuously and the test would pass while covering nothing.
        if List.isEmpty observed then
            failwith "no `calli` instruction was executed; this test no longer covers what it claims"

        observed, exitCode

    [<Test>]
    let ``calli reports SuspendedForClassInit when the callee suspends`` () =
        // `Activator.CreateInstance<T>()` is intrinsic; T's cctor has not run when the first call
        // is made, so the intrinsic pushes it and asks for the calli to be retried.
        let source =
            """
using System;

class C
{
    public static int Side;
    static C() { Side = 5; }
    public C() { }
}

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        delegate*<C> p = &Activator.CreateInstance<C>;
        C c = p();
        return (c != null && C.Side == 5) ? 0 : 1;
    }
}
"""

        let observed, exitCode = calliOutcomes "CalliSuspensionOutcome.cs" source

        // The guest checks that the instance was created and that C's cctor really ran, so the
        // outcomes below are being asserted about a run that actually did the right thing.
        exitCode |> shouldEqual 0

        // The first execution finds C's cctor un-run, so it suspends; the retry then commits.
        observed |> shouldContain WhatWeDid.SuspendedForClassInit
        observed |> shouldContain WhatWeDid.Executed

    /// The companion to the test above, and the reason the check cannot simply be "our program
    /// counter did not move". An intrinsic that *raises* also leaves the PC unadvanced — on
    /// purpose, so exception dispatch sees the faulting instruction's offset — while pushing the
    /// exception's constructor. That is not a retry: nothing will re-execute the `calli`, and
    /// `IlMachineStateExecution`'s own comment on that path says the outcome "is always
    /// `Executed`". Reporting a suspension here would tell the scheduler to keep threads parked
    /// behind a class initialisation that is not running.
    [<Test>]
    let ``calli reports Executed when the callee is an intrinsic that raises`` () =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

public static unsafe class Program
{
    public static int Main(string[] args)
    {
        delegate*<ref byte, int> p = &Unsafe.ReadUnaligned<int>;
        ref byte r = ref Unsafe.NullRef<byte>();

        int caught = 0;
        try { int v = p(ref r); } catch (NullReferenceException) { caught = 1; }
        return caught == 1 ? 0 : 1;
    }
}
"""

        let observed, exitCode = calliOutcomes "CalliIntrinsicRaises.cs" source

        // The guest caught the NullReferenceException the intrinsic raised.
        exitCode |> shouldEqual 0

        // Assert what the name promises, not just the absence of the wrong answer: every `calli`
        // step here must report `Executed`.
        observed |> shouldEqual (observed |> List.map (fun _ -> WhatWeDid.Executed))
        observed |> shouldNotContain WhatWeDid.SuspendedForClassInit
