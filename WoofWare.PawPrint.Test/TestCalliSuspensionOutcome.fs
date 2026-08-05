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

    [<Test>]
    let ``calli reports SuspendedForClassInit when the callee suspends`` () =
        // `Activator.CreateInstance<T>()` is intrinsic; T's cctor has not run when the first
        // call is made, so the intrinsic pushes it and asks for the calli to be retried.
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

        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", "CalliSuspensionOutcome.cs" ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestCalliSuspensionOutcome"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some "CalliSuspensionOutcome.cs")
                peImage
                dotnetRuntimes
                KernelConfig.Default
                None
                []
        with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        // Outcomes of exactly those steps that executed a `calli`.
        let rec loop (prepared : Program.PreparedProgram) (calliOutcomes : WhatWeDid list) : WhatWeDid list =
            let about =
                currentIlOp prepared.LastRan prepared.State
                |> Option.orElseWith (fun () -> currentIlOp prepared.EntryThread prepared.State)

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed _ -> List.rev calliOutcomes
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p calliOutcomes
            | Program.ProgramStepOutcome.InstructionStepped (p, _ran, whatWeDid) ->
                let calliOutcomes =
                    match about with
                    | Some op when isCalli op -> whatWeDid :: calliOutcomes
                    | _ -> calliOutcomes

                loop p calliOutcomes

        let calliOutcomes = loop prepared []

        // Guard the guard: if no `calli` step were observed at all, every assertion below would
        // hold vacuously and the test would pass while covering nothing.
        if List.isEmpty calliOutcomes then
            failwith "no `calli` instruction was executed; this test no longer covers what it claims"

        // The first execution of the `calli` finds C's cctor un-run, so it suspends; the retry
        // after the cctor returns then commits.
        calliOutcomes |> shouldContain WhatWeDid.SuspendedForClassInit
        calliOutcomes |> shouldContain WhatWeDid.Executed
