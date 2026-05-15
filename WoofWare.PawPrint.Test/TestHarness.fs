namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

/// Result of executing (some steps of) the program under PawPrint.
type RunResult =
    {
        /// Value that was left on the evaluation stack when execution stopped, **if**
        /// the program executed a `ret` that produced a value and PawPrint
        /// subsequently pushed it onto the stack.  This is only an early-stage
        /// approximation: once PawPrint supports a proper process-exit story we
        /// can promote this to a real exit–code.
        ExitCode : int option

        /// Final interpreter state after we stopped executing.
        FinalState : IlMachineState
    }

[<RequireQualifiedAccess>]
module MockEnv =
    /// Deterministic `NativeImpls` for tests: invariant-globalization is already
    /// seeded by `EmulatedKernel.defaultEnvironment`, so this mock only needs to
    /// cover the *behavioural* surface (processor count, managed thread id,
    /// FailFast). Guest env vars beyond the invariant default are supplied via
    /// the `env` argument to `Program.run` / `Program.prepare`, not here.
    let make () : NativeImpls =
        {
            System_Environment =
                { System_EnvironmentMock.Empty with
                    GetProcessorCount =
                        fun thread state ->
                            state
                            |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 1) thread
                            |> Tuple.withRight WhatWeDid.Executed
                            |> ExecutionResult.stepped
                    GetCurrentManagedThreadId =
                        fun thread state ->
                            state
                            |> IlMachineState.pushToEvalStack'
                                (EvalStackValue.Int32 (IlMachineState.getCurrentManagedThreadId thread state))
                                thread
                            |> Tuple.withRight WhatWeDid.Executed
                            |> ExecutionResult.stepped
                    // Surface FailFast as the abort outcome instead of raising
                    // NotImplementedException from the generated mock; the test
                    // harness then reports the guest-supplied diagnostic message,
                    // which is far more useful than a generic "Unimplemented mock
                    // function: FailFast" stack trace.
                    FailFast =
                        fun thread message _errorSource state -> ExecutionResult.FailFast (state, thread, message)
                }
        }

type EndToEndTestCase =
    {
        FileName : string
        ExpectedReturnCode : int
        NativeImpls : NativeImpls
        /// Guest environment overlay passed to `Program.run`. Layered on top
        /// of `EmulatedKernel.defaultEnvironment` so the
        /// invariant-globalization default is always present even when this
        /// map is empty.
        Environment : Map<string, string>
        ExpectsUnhandledException : bool
        /// Optional assertion run against the final PawPrint state once the
        /// guest has exited. Used by impure tests that want to verify
        /// interpreter-internal state (e.g. `state.Kernel.OutputLog`) that
        /// isn't observable as an exit code. Pure tests, which run the
        /// same source on the real CLR for cross-comparison, leave this
        /// `None` — the real runtime has no analogous state to assert
        /// against.
        AssertTerminalState : (IlMachineState -> unit) option
    }
