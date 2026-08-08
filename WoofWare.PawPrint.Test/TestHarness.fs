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
        /// Kernel configuration passed to `Program.run`: guest environment
        /// overlay (layered on top of `EmulatedKernel.defaultEnvironment`, so
        /// the invariant-globalization default is always present even when the
        /// overlay is empty) plus the reported processor count.
        KernelConfig : KernelConfig
        /// AppContext properties to seed before the guest starts, as a real host
        /// does from `runtimeconfig.json`. Must be empty for any case run through
        /// the *pure* (differential) harness: the oracle there loads the guest
        /// in-process on the host runtime, whose `AppContext` is already set up
        /// and cannot be reseeded, so a seeded property is a PawPrint-only fact
        /// and belongs in `sourcesImpure`. `TestPureCases.runTest` enforces this.
        AppContext : AppContextProperties
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
