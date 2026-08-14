namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins that a `calli` commits exactly once, and that a class initialisation it triggers is
/// reported against the *callee's* frame rather than against the `calli`.
///
/// A call used to be able to decline to commit: a callee needing its class initialiser run
/// pushed that cctor frame and left the calling instruction's PC unadvanced so the instruction
/// re-executed. `calli` was the one op that had to prepare for that retry, because its callee
/// comes off the evaluation stack and the function pointer sits above the arguments — so it had
/// to pop the pointer before calling in, and push it back if the call did not happen. Getting
/// that wrong stranded the pointer and the retry failed with "expected a function pointer on
/// top".
///
/// Class initialisation is now the callee's own prologue, which runs after the frame is pushed,
/// so no instruction ever re-executes and there is no pointer to restore. The tests below pin
/// both halves of that: the `calli` steps report `Executed`, and the suspension appears on a
/// step executing `C..ctor` — which is also where the CLR puts the check, and why a `.cctor`
/// that throws names the constructor in its `TypeInitializationException`.
///
/// Asserted by stepping the guest and attributing each outcome to the frame that produced it, so
/// an unrelated class init elsewhere in start-up cannot satisfy any of it.
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

    /// One step of the guest: which method was executing when it ran, whether the instruction it
    /// was about to run was a `calli`, and what the scheduler was told.
    type private Step =
        {
            DeclaringType : string
            Method : string
            WasCalli : bool
            Outcome : WhatWeDid
        }

    /// The name of the method the given thread is executing, for attributing a step to a frame.
    let private currentMethod (thread : ThreadId) (state : IlMachineState) : (string * string) option =
        match state.ThreadState |> Map.tryFind thread with
        | None -> None
        | Some threadState ->
            let m = threadState.MethodState.ExecutingMethod
            Some (m.RequiredDeclaringType.Name, m.Name)

    /// Steps the guest to completion, returning one entry per step together with the exit code.
    ///
    /// Attributing each outcome to its frame matters: class initialisation happens all over
    /// start-up, so a test that looked at every step undifferentiated could be satisfied by
    /// something entirely unrelated to the instruction under test. The exit code matters too —
    /// without it, a run that reported the right outcomes while corrupting the guest's state would
    /// still pass.
    let private calliOutcomes (sourceName : string) (source : string) : Step list * int =
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
        let rec loop (prepared : Program.PreparedProgram) (acc : Step list) : Step list * int =
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
                    match currentMethod ran stateBefore with
                    | None -> acc
                    | Some (declaringType, method) ->
                        let wasCalli =
                            match currentIlOp ran stateBefore with
                            | Some op -> isCalli op
                            | None -> false

                        {
                            DeclaringType = declaringType
                            Method = method
                            WasCalli = wasCalli
                            Outcome = whatWeDid
                        }
                        :: acc

                loop p acc

        let observed, exitCode = loop prepared []

        // Guard the guard: with no `calli` step observed, every assertion below would hold
        // vacuously and the test would pass while covering nothing.
        if observed |> List.forall (fun s -> not s.WasCalli) then
            failwith "no `calli` instruction was executed; this test no longer covers what it claims"

        observed, exitCode

    [<Test>]
    let ``calli commits, and its callee's class init is reported against the callee`` () =
        // `Activator.CreateInstance<T>()` is intrinsic and allocates T, then calls T's ctor; T's
        // cctor has not run at that point, so the ctor's prologue runs it.
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

        // The `calli` commits on its only execution. It is not retried, which is what makes the
        // function pointer it popped safe to discard.
        let calliOutcomes = observed |> List.filter _.WasCalli |> List.map _.Outcome

        calliOutcomes
        |> shouldEqual (calliOutcomes |> List.map (fun _ -> WhatWeDid.Executed))

        // C's initialisation is reported against C's constructor, whose prologue asked for it.
        let ctorSuspensions =
            observed
            |> List.filter (fun s ->
                s.DeclaringType = "C"
                && s.Method = ".ctor"
                && s.Outcome = WhatWeDid.SuspendedForClassInit
            )

        ctorSuspensions |> List.length |> shouldEqual 1

    /// The companion to the test above: a `calli` whose callee raises rather than running still
    /// reports `Executed`. The intrinsic leaves the PC unadvanced — on purpose, so exception
    /// dispatch sees the faulting instruction's offset — while pushing the exception's
    /// constructor, and that must not be mistaken for a call that did not happen.
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
        let calliOutcomes = observed |> List.filter _.WasCalli |> List.map _.Outcome

        calliOutcomes
        |> shouldEqual (calliOutcomes |> List.map (fun _ -> WhatWeDid.Executed))

        calliOutcomes |> shouldNotContain WhatWeDid.SuspendedForClassInit
