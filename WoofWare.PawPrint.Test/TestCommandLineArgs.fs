namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open NUnit.Framework
open FsUnitTyped
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `Environment.GetCommandLineArgs()` and `Main`'s arguments come from one call to CoreLib's
/// `Environment.InitializeCommandLineArgs`, which PawPrint runs during startup the way
/// `CorHost2::ExecuteAssembly` does. `sourcesPure/CommandLineArgs.cs` pins the relationship
/// between the two arrays, but the pure harness launches every guest with no arguments, so it
/// can only ever see the length-1 case and its tail comparison is vacuous.
///
/// These run the same guest under both runtimes with a *non-empty* argument list, which is
/// what makes the tail assertions load-bearing: an implementation that returned the program
/// name alone, or that dropped it, or that built `Main`'s arguments separately from
/// `s_commandLineArgs`, passes the pure case and fails here.
[<TestFixture>]
[<Category("Guest")>]
[<Explicit "Runs a guest end-to-end under the interpreter">]
module TestCommandLineArgs =

    /// Arguments chosen so that neither position can be confused for the other and neither is
    /// a prefix of the other: an off-by-one in the copy loop changes the exit code rather than
    /// producing a differently-ordered but equal-looking array.
    let private guestArgs = [ "alpha" ; "beta" ; "gamma" ]

    let private guestSource =
        """
using System;

class CommandLineArgsWithArguments
{
    static int Main(string[] args)
    {
        string[] a = Environment.GetCommandLineArgs();

        // The program name, then every argument, in order.
        if (a.Length != args.Length + 1)
        {
            return 11;
        }

        if (args.Length != 3)
        {
            return 12;
        }

        if (args[0] != "alpha" || args[1] != "beta" || args[2] != "gamma")
        {
            return 13;
        }

        for (int i = 0; i < args.Length; i++)
        {
            if (a[i + 1] != args[i])
            {
                return 14;
            }
        }

        // The two arrays are built by one pass over one input, so the arguments are not merely
        // equal but the *same* string objects. A runtime that built them separately would pass
        // the comparison above and fail this.
        for (int i = 0; i < args.Length; i++)
        {
            if (!ReferenceEquals(a[i + 1], args[i]))
            {
                return 15;
            }
        }

        if (string.IsNullOrEmpty(a[0]))
        {
            return 16;
        }

        return 0;
    }
}
"""

    /// The exit code a `RunOutcome` carries, read the way the App reads it.
    let private exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, thread) ->
            match state.ThreadState.[thread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
            | other -> failwith $"guest did not leave an int32 exit code on the stack: %O{other}"
        | other -> failwith $"guest did not exit normally: %O{other}"

    [<Test>]
    let ``Main's arguments and GetCommandLineArgs agree, under both runtimes`` () : unit =
        let image = Roslyn.compile [ guestSource ]
        let args = List.toArray guestArgs

        let realResult = RealRuntime.executeWithRealRuntime args image

        let realExitCode =
            match realResult with
            | RealRuntimeResult.NormalExit code -> code
            | other -> failwith $"real runtime did not exit normally: %O{other}"

        // Asserted rather than merely compared: if the guest ever stopped reaching its final
        // `return 0` on both runtimes, an equality-only test would still pass while checking
        // nothing. See the guest's own return codes for which assertion failed.
        realExitCode |> shouldEqual 0

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use loggerFactory =
            LoggerFactory.Create (fun b -> b.SetMinimumLevel LogLevel.Warning |> ignore)

        use peImage = new MemoryStream (image : byte[])

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Argv = guestArgs
                    }
            }

        let pawPrintExitCode =
            Program.run loggerFactory (Some "CommandLineArgsWithArguments.cs") peImage hostConfig
            |> exitCodeOf

        pawPrintExitCode |> shouldEqual realExitCode
