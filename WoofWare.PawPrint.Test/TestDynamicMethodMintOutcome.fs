namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Pins the *shape* of minting a `DynamicMethod` a `call` names: the instruction runs twice, the
/// first run reporting `SuspendedForManagedCall` because it pushed the guest's
/// `GetMethodDescriptor` on top of itself, and the second `Executed` because the mint has
/// happened and the callee's frame goes on.
///
/// Neither half shows up in the guest's answer, which is why this exists. A run that reported
/// `Executed` for the suspension would compute exactly the same result and pass every end-to-end
/// test, while telling the scheduler that this thread finished a step — which wakes any thread
/// parked on it for class initialisation (`Scheduler.onStepOutcome`, the `wakeClassInitWaiters`
/// call on the `Executed` arm). That is a change to the interleaving, and PawPrint exists to make
/// interleavings reproducible.
///
/// Modelled on `TestCalliSuspensionOutcome`, and attributing each outcome to the instruction that
/// produced it for the same reason: suspensions happen all over start-up, so a test that looked at
/// every step undifferentiated could be satisfied by something unrelated.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDynamicMethodMintOutcome =

    let private assy = typeof<RunResult>.Assembly

    /// `outer` calls `inner`, and nothing ever mints `inner`: no `CreateDelegate`, no `Invoke`.
    /// Deliberately the smallest such program, so that the single `call` against a `DynamicScope`
    /// in the whole run is the one under test.
    let private callsUnmintedCallee =
        """
using System;
using System.Reflection.Emit;

class CallsUnmintedCallee
{
    static int Main()
    {
        DynamicMethod inner = new DynamicMethod("Inner", typeof(int), new Type[] { typeof(int) }, typeof(CallsUnmintedCallee).Module);
        ILGenerator ii = inner.GetILGenerator();
        ii.Emit(OpCodes.Ldarg_0);
        ii.Emit(OpCodes.Ldc_I4_1);
        ii.Emit(OpCodes.Add);
        ii.Emit(OpCodes.Ret);

        DynamicMethod outer = new DynamicMethod("Outer", typeof(int), new Type[0], typeof(CallsUnmintedCallee).Module);
        ILGenerator il = outer.GetILGenerator();
        il.Emit(OpCodes.Ldc_I4, 41);
        il.Emit(OpCodes.Call, inner);
        il.Emit(OpCodes.Ret);

        Func<int> f = (Func<int>) outer.CreateDelegate(typeof(Func<int>));
        return f() == 42 ? 0 : 1;
    }
}
"""

    /// True when the thread is about to execute a `call` whose operand names a `DynamicScope`
    /// entry rather than a metadata row. The operand kind is the discriminator, not the opcode:
    /// start-up is full of ordinary `call`s.
    let private isScopeCall (thread : ThreadId) (state : IlMachineState) : bool =
        match state.ThreadState |> Map.tryFind thread with
        | None -> false
        | Some threadState ->
            match MethodBody.tryIl threadState.MethodState.ExecutingMethod.Body with
            | None -> false
            | Some instructions ->
                match instructions.Locations.TryGetValue threadState.MethodState.IlOpIndex with
                | true, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromDynamicScope _) ->
                    true
                | _ -> false

    /// The outcome of every step whose pre-step instruction was a `call` against a `DynamicScope`,
    /// in order, together with the guest's exit code.
    let private scopeCallOutcomes (sourceName : string) (source : string) : WhatWeDid list * int =
        let image = Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestDynamicMethodMintOutcome"

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        // Reflection.Emit is off by default under PawPrint, so the guest would otherwise die of
        // `PlatformNotSupportedException` before emitting anything.
        let hostConfig = HostConfig.Default dotnetRuntimes

        let hostConfig =
            { hostConfig with
                Guest =
                    { hostConfig.Guest with
                        AppContext =
                            AppContextProperties.ofMap (
                                Map.ofList
                                    [
                                        "System.Runtime.CompilerServices.RuntimeFeature.IsDynamicCodeSupported", "true"
                                    ]
                            )
                    }
            }

        match Program.prepare loggerFactory (Some sourceName) peImage hostConfig with
        | Program.ProgramStartResult.CompletedBeforeMain outcome -> failwith $"guest completed before Main: %O{outcome}"
        | Program.ProgramStartResult.Ready prepared ->

        let rec loop (prepared : Program.PreparedProgram) (acc : WhatWeDid list) : WhatWeDid list * int =
            let stateBefore = prepared.State

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.Completed outcome ->
                match outcome with
                | RunOutcome.NormalExit (terminalState, _)
                | RunOutcome.ProcessExit (terminalState, _) -> List.rev acc, terminalState.LatchedExitCode
                | other -> failwith $"guest did not exit normally: %O{other}"
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"guest deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.WorkerTerminated (p, _) -> loop p acc
            | Program.ProgramStepOutcome.InstructionStepped (p, ran, whatWeDid, _) ->
                let acc =
                    if isScopeCall ran stateBefore then
                        whatWeDid :: acc
                    else
                        acc

                loop p acc

        loop prepared []

    [<Test>]
    let ``a call naming an unminted dynamic method suspends once, then runs`` () : unit =
        let outcomes, exitCode =
            scopeCallOutcomes "CallsUnmintedCallee.cs" callsUnmintedCallee

        // The exit code first: without it, a run that reported the right outcomes while computing
        // the wrong answer — or while calling the wrong method — would still pass.
        exitCode |> shouldEqual 0

        // Exactly two, and in this order. Two is what says the instruction re-executed rather than
        // being skipped; the order is what says the mint came first. Anything else — one step, or
        // three — means the suspension and the re-execution have come apart.
        outcomes
        |> shouldEqual [ WhatWeDid.SuspendedForManagedCall ; WhatWeDid.Executed ]
