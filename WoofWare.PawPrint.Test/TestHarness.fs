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

type NativeImpls =
    {
        System_Env : ISystem_Environment
    }

    interface ISystem_Environment_Env with
        member this.System_Environment = this.System_Env

    static member Mock () =
        {
            System_Env = System_EnvironmentMock.Empty
        }

type TestCase =
    {
        FileName : string
        ExpectedReturnCode : int
        NativeImpls : NativeImpls
    }
