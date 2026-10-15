namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `RealRuntime.executeConcurrently`: its guests run at the same time, in one working directory,
/// on the frameworks and with the AppContext properties they are given; and
/// `FrameworkUnderTest.aspNetCoreFrameworkDirectory`, which it runs ASP.NET Core guests on.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRealRuntimeConcurrent =

    /// Writes `mine`, then waits for `theirs`, which only a guest running at the same time can
    /// write; exits 0 once it appears, or 1 after 30 seconds.
    let private rendezvousSource (mine : string) (theirs : string) : string =
        $$"""
using System;
using System.IO;
using System.Threading;

public static class Program
{
    public static int Main()
    {
        File.WriteAllText("{{mine}}", "");
        for (int i = 0; i < 3000; i++)
        {
            if (File.Exists("{{theirs}}"))
            {
                Console.WriteLine("{{mine}} met {{theirs}}");
                return 0;
            }
            Thread.Sleep(10);
        }
        return 1;
    }
}
"""

    let private guest
        (assemblyName : string)
        (source : string)
        (references : MetadataReference list)
        : ConcurrentGuest
        =
        {
            Image = Roslyn.compileAssembly assemblyName OutputKind.ConsoleApplication references [ source ]
            Args = []
        }

    let private exitAndStdout (run : CapturedRun) : RealRuntimeResult * string = run.Result, run.Stdout

    [<Test>]
    let ``guests run at the same time, in one working directory`` () : unit =
        RealRuntime.executeConcurrently
            OracleFrameworks.NetCoreApp
            AppContextProperties.empty
            []
            [
                guest "RendezvousFirst" (rendezvousSource "first" "second") []
                guest "RendezvousSecond" (rendezvousSource "second" "first") []
            ]
        |> List.map exitAndStdout
        |> shouldEqual
            [
                RealRuntimeResult.NormalExit 0, "first met second\n"
                RealRuntimeResult.NormalExit 0, "second met first\n"
            ]

    /// Exits with `code`.
    let private exitsWithSource (code : int) : string =
        $$"""
public static class Program
{
    public static int Main() => {{code}};
}
"""

    [<Test>]
    let ``guests sharing an assembly name each run as themselves`` () : unit =
        // One name exactly, and one differing only in case, which aliases on a case-insensitive
        // filesystem such as macOS's default.
        RealRuntime.executeConcurrently
            OracleFrameworks.NetCoreApp
            AppContextProperties.empty
            []
            [
                guest "Same" (exitsWithSource 1) []
                guest "Same" (exitsWithSource 2) []
                guest "SAME" (exitsWithSource 3) []
            ]
        |> List.map _.Result
        |> shouldEqual
            [
                RealRuntimeResult.NormalExit 1
                RealRuntimeResult.NormalExit 2
                RealRuntimeResult.NormalExit 3
            ]

    /// Prints the name of the assembly that defines `WebApplication`, which binds only if the
    /// runtimeconfig names ASP.NET Core.
    let private aspNetCoreSource : string =
        """
public static class Program
{
    public static int Main()
    {
        System.Console.WriteLine(typeof(Microsoft.AspNetCore.Builder.WebApplication).Assembly.GetName().Name);
        return 0;
    }
}
"""

    let private aspNetCoreGuest () : ConcurrentGuest =
        let reference =
            Path.Combine (HostImage.aspNetCoreFrameworkDirectory, "Microsoft.AspNetCore.dll")
            |> MetadataReference.CreateFromFile

        guest "UsesAspNetCore" aspNetCoreSource [ reference ]

    [<Test>]
    let ``an ASP.NET Core guest runs when ASP.NET Core is among the frameworks, and not otherwise`` () : unit =
        let image = aspNetCoreGuest ()

        RealRuntime.executeConcurrently
            OracleFrameworks.NetCoreAppAndAspNetCoreApp
            AppContextProperties.empty
            []
            [ image ]
        |> List.map exitAndStdout
        |> shouldEqual [ RealRuntimeResult.NormalExit 0, "Microsoft.AspNetCore\n" ]

        match RealRuntime.executeConcurrently OracleFrameworks.NetCoreApp AppContextProperties.empty [] [ image ] with
        | [ {
                Result = RealRuntimeResult.UnhandledException report
            } ] -> report |> shouldContainText "Microsoft.AspNetCore"
        | other -> failwith $"expected the guest to fail to bind ASP.NET Core, got %A{other}"

    [<Test>]
    let ``the AppContext properties reach the guests`` () : unit =
        let source =
            """
public static class Program
{
    public static int Main()
    {
        System.Console.WriteLine(System.AppContext.GetData("WoofWare.Test.Property"));
        return 0;
    }
}
"""

        RealRuntime.executeConcurrently
            OracleFrameworks.NetCoreApp
            (AppContextProperties.ofMap (Map.ofList [ "WoofWare.Test.Property", "a \"quoted\" value" ]))
            []
            [ guest "ReadsProperty" source [] ]
        |> List.map exitAndStdout
        |> shouldEqual [ RealRuntimeResult.NormalExit 0, "a \"quoted\" value\n" ]

    [<Test>]
    let ``the ASP.NET Core directory is a Microsoft.AspNetCore.App framework on the runtime-dir list`` () : unit =
        let dir = FrameworkUnderTest.aspNetCoreFrameworkDirectory ()

        FrameworkUnderTest.runtimeDirs () |> Seq.contains dir |> shouldEqual true
        File.Exists (Path.Combine (dir, "Microsoft.AspNetCore.dll")) |> shouldEqual true

        dir
        |> Path.TrimEndingDirectorySeparator
        |> Path.GetDirectoryName
        |> Path.GetFileName
        |> shouldEqual "Microsoft.AspNetCore.App"
