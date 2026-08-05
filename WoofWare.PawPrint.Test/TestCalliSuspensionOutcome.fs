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
/// The difference is not cosmetic. `Scheduler.onStepOutcome` treats `Executed` as forward
/// progress and wakes every thread parked `BlockedOnClassInit` on this one, whereas
/// `SuspendedForClassInit` deliberately leaves them parked because the class init has *not*
/// finished. Reporting `Executed` here would wake a waiter while the cctor is still running:
/// it would take a step, re-block, and the interleaving would differ from the one the
/// scheduler intended. In a runtime whose purpose is reproducible thread schedules, a
/// spurious wake is a correctness bug and not merely a wasted step.
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
    /// steps whose executing instruction was a `calli`. Restricting to `calli` steps matters:
    /// class initialisation happens all over start-up, so a test that looked at every step could
    /// be satisfied by something entirely unrelated to the instruction under test.
    let private calliOutcomes (sourceName : string) (source : string) : WhatWeDid list =
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

        let rec loop (prepared : Program.PreparedProgram) (acc : WhatWeDid list) : WhatWeDid list =
            let about =
                currentIlOp prepared.LastRan prepared.State
                |> Option.orElseWith (fun () -> currentIlOp prepared.EntryThread prepared.State)

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed _ -> List.rev acc
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p acc
            | Program.ProgramStepOutcome.InstructionStepped (p, _ran, whatWeDid) ->
                let acc =
                    match about with
                    | Some op when isCalli op -> whatWeDid :: acc
                    | _ -> acc

                loop p acc

        let observed = loop prepared []

        // Guard the guard: with no `calli` step observed, every assertion below would hold
        // vacuously and the test would pass while covering nothing.
        if List.isEmpty observed then
            failwith "no `calli` instruction was executed; this test no longer covers what it claims"

        observed

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

        let observed = calliOutcomes "CalliSuspensionOutcome.cs" source

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

        let observed = calliOutcomes "CalliIntrinsicRaises.cs" source

        observed |> shouldNotContain WhatWeDid.SuspendedForClassInit
