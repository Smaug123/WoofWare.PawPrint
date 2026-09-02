namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open Microsoft.FSharp.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Pins the conversion that lets an *IL instruction* end the process: by aborting, or by raising
/// an exception that nothing on its thread handles.
///
/// The other terminating outcomes never need one: `Environment.FailFast` and `Environment._Exit`
/// are native handlers, and a native handler returns `NativeHandlerResult.Terminating` with an
/// `ExecutionResult` already in hand. An opcode has only a `WhatWeDid` to report with, and
/// `WhatWeDid` carries no `ThreadId` — so `AbstractMachine.surfaceTerminatingStep` is where the
/// thread that gave up is attached, at the single exit from `executeOneStep`.
///
/// The abort half is tested directly because nothing constructs `WhatWeDid.Aborted` yet: the gate
/// that will is a separate change, and a conversion with no producer is otherwise pinned by
/// nothing at all. The unhandled-exception half has producers (a static-field opcode or method
/// prologue rethrowing a cached `TypeInitializationException`), and the `sourcesPure` guests
/// named `CachedCctorFailureUnhandled*` pin it end to end.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAbortChannel =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    let private fatal : FatalError =
        {
            Code = FatalErrorCode.ExecutionEngine
            Message = Some "boom"
        }

    /// An unhandled exception for these tests to carry. Its contents are arbitrary: the conversion
    /// under test inspects the case and passes the payload through.
    let private unhandled () : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> =
        {
            ExceptionObject = ManagedHeapAddress 1
            StackTrace = []
        }

    [<Test>]
    let ``an aborting step becomes a terminating outcome naming the thread`` () : unit =
        let thread = ThreadId 3

        let converted =
            ExecutionResult.Stepped (state (), WhatWeDid.Aborted fatal, StepEffect.NoEffect)
            |> AbstractMachine.surfaceTerminatingStep thread

        match converted with
        | ExecutionResult.Aborted (_, abortingThread, f) ->
            abortingThread |> shouldEqual thread
            f |> shouldEqual fatal
        | other -> failwith $"expected an aborting step to become ExecutionResult.Aborted, got %O{other}"

    [<Test>]
    let ``a step whose exception nothing handles becomes a terminating outcome naming the thread`` () : unit =
        let thread = ThreadId 3
        let exn = unhandled ()

        let converted =
            ExecutionResult.Stepped (state (), WhatWeDid.UnhandledException exn, StepEffect.NoEffect)
            |> AbstractMachine.surfaceTerminatingStep thread

        match converted with
        | ExecutionResult.UnhandledException (_, terminatingThread, e) ->
            terminatingThread |> shouldEqual thread
            e |> shouldEqual exn
        | other ->
            failwith
                $"expected a step with an unhandled exception to become ExecutionResult.UnhandledException, got %O{other}"

    [<Test>]
    let ``every other WhatWeDid passes through untouched`` () : unit =
        // The conversion must be keyed on the two terminating variants and nothing else: a
        // `Stepped` that it rewrote would rob the scheduler of the outcome it needs to do its
        // bookkeeping, and one it swallowed would stop the thread retiring its step at all.
        let variants : WhatWeDid list =
            [
                WhatWeDid.Executed
                // One entry per case, so that the arity tie below is a real count. The payloads
                // are arbitrary: this conversion inspects the case and nothing else.
                WhatWeDid.VoluntaryYield true
                WhatWeDid.SuspendedForClassInit
                WhatWeDid.SuspendedForManagedCall
                WhatWeDid.BlockedOnClassInit (ThreadId 1)
                WhatWeDid.ThrowingTypeInitializationException
            ]

        // Tie the hand-written table to the type: a variant added later must be classified here
        // rather than silently inheriting the pass-through arm. The abort and unhandled-exception
        // cases are the two deliberately absent, hence the `+ 2`.
        FSharpType.GetUnionCases typeof<WhatWeDid>
        |> Array.length
        |> shouldEqual (variants.Length + 2)

        for variant in variants do
            let stepped = ExecutionResult.Stepped (state (), variant, StepEffect.NoEffect)

            match AbstractMachine.surfaceTerminatingStep (ThreadId 0) stepped with
            | ExecutionResult.Stepped (_, whatWeDid, effect) ->
                whatWeDid |> shouldEqual variant
                effect |> shouldEqual StepEffect.NoEffect
            | other -> failwith $"expected %O{variant} to pass through unchanged, got %O{other}"

    [<Test>]
    let ``an outcome that is not a step passes through untouched`` () : unit =
        let thread = ThreadId 0
        let s = state ()

        let alreadyTerminating = ExecutionResult.ProcessExit (s, thread)

        match AbstractMachine.surfaceTerminatingStep thread alreadyTerminating with
        | ExecutionResult.ProcessExit (_, t) -> t |> shouldEqual thread
        | other -> failwith $"expected a non-Stepped outcome to pass through unchanged, got %O{other}"

    [<Test>]
    let ``an aborting step that also requests an effect is refused`` () : unit =
        // A step that tore the process down did not finish whatever it was describing, so an
        // effect here would ask the driver to perform a write on behalf of a step that never
        // completed. No producer does this; refusing beats silently choosing between dropping the
        // effect and performing it.
        let effect =
            StepEffect.WroteToFd (FileDescriptorRole.StandardOutput, ImmutableArray.Create<byte> 1uy)

        let stepped = ExecutionResult.Stepped (state (), WhatWeDid.Aborted fatal, effect)

        let e =
            Assert.Throws (fun () -> AbstractMachine.surfaceTerminatingStep (ThreadId 0) stepped |> ignore)

        if not (e.Message.Contains "must not emit one") then
            failwith $"expected the refusal to name the rule it enforces, got: %s{e.Message}"

    [<Test>]
    let ``a step with an unhandled exception that also requests an effect is refused`` () : unit =
        // Same rule as for an abort: the thread unwound to nothing, so the step never completed
        // and has no effect to ask for.
        let effect =
            StepEffect.WroteToFd (FileDescriptorRole.StandardOutput, ImmutableArray.Create<byte> 1uy)

        let stepped =
            ExecutionResult.Stepped (state (), WhatWeDid.UnhandledException (unhandled ()), effect)

        let e =
            Assert.Throws (fun () -> AbstractMachine.surfaceTerminatingStep (ThreadId 0) stepped |> ignore)

        if not (e.Message.Contains "must not emit one") then
            failwith $"expected the refusal to name the rule it enforces, got: %s{e.Message}"
