namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open NUnit.Framework
open FsUnitTyped
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `Environment.GetCommandLineArgs()` and `Main`'s arguments come from one call to CoreLib's
/// `Environment.InitializeCommandLineArgs`, which PawPrint runs during startup the way
/// `CorHost2::ExecuteAssembly` does. `sourcesPure/CommandLineArgs.cs` pins the relationship
/// between the two arrays, but the pure harness launches every guest with no arguments, so it
/// can only ever see the length-1 case and its tail comparison is vacuous.
///
/// The differential here runs the same guest under both runtimes with a *non-empty* argument
/// list, which is what makes the tail assertions load-bearing: an implementation that returned
/// the program name alone, or that dropped it, or that built `Main`'s arguments separately
/// from `s_commandLineArgs`, passes the pure case and fails here.
///
/// The rest pin what element 0 is, which has no cross-runtime oracle — the real runtime reports
/// whatever path launched the test host — so they assert the exact bytes a configured
/// `GuestConfig.AssemblyPath` produces, and the exact bytes its absence falls back to.
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

    /// A guest that reports nothing but the program name it was given, so that a test can
    /// assert the exact bytes rather than a shape.
    let private echoArgv0Source =
        """
using System;

class EchoArgv0
{
    static int Main()
    {
        Console.Write(Environment.GetCommandLineArgs()[0]);
        return 0;
    }
}
"""

    /// Run `image` under PawPrint with `assemblyPath` as the host's name for it, and return
    /// what the guest wrote to stdout.
    let private argv0Reported (assemblyPath : string option) (image : byte[]) : string =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use loggerFactory =
            LoggerFactory.Create (fun b -> b.SetMinimumLevel LogLevel.Warning |> ignore)

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        AssemblyPath = assemblyPath
                    }
            }

        // `originalPath` is deliberately something that is not an assembly path at all: it is
        // where the host read the image from, and a test that let it reach the guest would be
        // asserting the very conflation `GuestConfig.AssemblyPath` exists to prevent.
        match Program.run loggerFactory (Some "EchoArgv0.cs") peImage hostConfig with
        | RunOutcome.NormalExit (state, _) as outcome ->
            exitCodeOf outcome |> shouldEqual 0

            OutputLogEntry.bytesFor FileDescriptorRole.StandardOutput state.Kernel.OutputLog
            |> Seq.toArray
            |> Text.Encoding.UTF8.GetString
        | other -> failwith $"guest did not exit normally: %O{other}"

    [<Test>]
    let ``A host-named assembly path reaches the guest verbatim`` () : unit =
        // Not resolved against anything, and deliberately absent from the emulated filesystem:
        // CoreCLR forwards `pwzAssemblyPath` to `InitializeCommandLineArgs` without consulting
        // the disk, so what the host says is what the guest reads.
        let named = "/opt/app/Guest.dll"

        Roslyn.compile [ echoArgv0Source ]
        |> argv0Reported (Some named)
        |> shouldEqual named

    [<Test>]
    let ``An unnamed assembly falls back to the name stamped into the image`` () : unit =
        // `Roslyn.compile` builds every guest as `PawPrintTestAssembly` with
        // `OutputKind.ConsoleApplication`, so the Module row records `.exe` — measured, not
        // assumed, and worth pinning: the extension comes from the image rather than from any
        // rule of PawPrint's, which is the whole point of reading `ScopeName` instead of
        // manufacturing a name from the assembly's simple name.
        //
        // Asserting the literal rather than re-deriving it from the image keeps this a claim
        // about *which* fallback was chosen: one that read the host's `originalPath` would
        // report "EchoArgv0.cs" and fail here.
        Roslyn.compile [ echoArgv0Source ]
        |> argv0Reported None
        |> shouldEqual "PawPrintTestAssembly.exe"

    /// Run `image` under PawPrint with the given command-line configuration, returning the
    /// message it refused with. Fails the test if the run is *accepted*.
    let private refusalFor (assemblyPath : string option) (argv : string list) (image : byte[]) : string =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use loggerFactory =
            LoggerFactory.Create (fun b -> b.SetMinimumLevel LogLevel.Warning |> ignore)

        use peImage = new MemoryStream (image)

        let hostConfig =
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        AssemblyPath = assemblyPath
                        Argv = argv
                    }
            }

        let exn =
            Assert.Throws (fun () ->
                Program.run loggerFactory (Some "EchoArgv0.cs") peImage hostConfig
                |> ignore<RunOutcome>
            )

        exn.Message

    [<Test>]
    let ``An argument containing a NUL is refused, naming which one`` () : unit =
        // No `execve` can produce this, so a host asking for it is describing a process that
        // cannot exist. The failure mode being guarded is *silent* truncation: the value would
        // otherwise reach the guest as "a", because the marshalled buffer is NUL-terminated and
        // CoreLib rebuilds each element with `new string(char*)`.
        let message =
            Roslyn.compile [ echoArgv0Source ] |> refusalFor None [ "fine" ; "a\000b" ]

        // The index localises it: a host with many arguments should not have to bisect.
        message |> shouldContainText "GuestConfig.Argv[1]"
        message |> shouldContainText "NUL at index 1"

    [<Test>]
    let ``An assembly path containing a NUL is refused`` () : unit =
        // The same rule on the other string that reaches the same buffer. Asserted separately
        // because it is a different knob with a different name in the message, so a guard
        // covering only the arguments would pass the test above and fail this.
        Roslyn.compile [ echoArgv0Source ]
        |> refusalFor (Some "/opt/app/Gu\000est.dll") []
        |> shouldContainText "GuestConfig.AssemblyPath"
