namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

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

type EndToEndTestCase =
    {
        FileName : string
        ExpectedReturnCode : int
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
