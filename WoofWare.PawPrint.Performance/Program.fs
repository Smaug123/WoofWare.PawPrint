namespace WoofWare.PawPrint.Performance

open System
open System.Collections.Immutable
open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.Extensions.Logging.Abstractions
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.ExternImplementations

[<RequireQualifiedAccess>]
module private Roslyn =
    let private metadataReferences (extraReferences : MetadataReference list) : MetadataReference[] =
        let runtimeDir = Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory ()

        let runtimeReferences =
            Directory.GetFiles (runtimeDir, "*.dll")
            |> Array.map (fun path -> MetadataReference.CreateFromFile path :> MetadataReference)

        Array.append runtimeReferences (extraReferences |> List.toArray)

    let compileAssembly
        (assemblyName : string)
        (outputKind : OutputKind)
        (extraReferences : MetadataReference list)
        (sources : string list)
        : byte[]
        =
        let parseOptions =
            CSharpParseOptions.Default.WithLanguageVersion LanguageVersion.Preview

        let syntaxTrees : SyntaxTree[] =
            sources
            |> List.mapi (fun idx src ->
                let fileName = $"File{idx}.cs"
                CSharpSyntaxTree.ParseText (src, parseOptions, fileName)
            )
            |> List.toArray

        let compilationOptions = CSharpCompilationOptions(outputKind).WithAllowUnsafe true

        let compilation =
            CSharpCompilation.Create (
                assemblyName = assemblyName,
                syntaxTrees = syntaxTrees,
                references = metadataReferences extraReferences,
                options = compilationOptions
            )

        use peStream = new MemoryStream ()

        let emitResult = compilation.Emit peStream

        if emitResult.Success then
            peStream.ToArray ()
        else
            let diagnostics =
                emitResult.Diagnostics
                |> Seq.filter (fun d -> d.Severity = DiagnosticSeverity.Error)
                |> Seq.map string
                |> String.concat Environment.NewLine

            failwith $"Compilation failed:\n%s{diagnostics}"

    let compile (sources : string list) : byte[] =
        compileAssembly "PawPrintPerformanceAssembly" OutputKind.ConsoleApplication [] sources

/// Result of executing the program using the real .NET runtime.
type private RealRuntimeResult =
    | NormalExit of exitCode : int
    | UnhandledException of exn : Exception

[<RequireQualifiedAccess>]
module private RealRuntime =
    let executeWithRealRuntime (args : string[]) (assemblyBytes : byte array) : RealRuntimeResult =
        let assy = System.Reflection.Assembly.Load assemblyBytes

        try
            let result = assy.EntryPoint.Invoke ((null : obj), [| args |]) |> unbox<int>
            RealRuntimeResult.NormalExit result
        with :? System.Reflection.TargetInvocationException as tie ->
            RealRuntimeResult.UnhandledException (tie.InnerException |> Option.ofObj |> Option.defaultValue (tie :> _))

[<RequireQualifiedAccess>]
module private GuestPrograms =
    let stackHeavy (guestIterations : int) : string =
        let template =
            """
public static class Program
{
    const int Iterations = __ITERATIONS__;

    static int Step(int a, int b, int c, int d, int i)
    {
        a = a + b;
        b = b + c;
        c = c + d;
        d = d + a;

        if ((i & 1) == 0)
        {
            a = a - c;
            d = d + 3;
        }
        else
        {
            b = b - d;
            c = c + 5;
        }

        return (a ^ b ^ c ^ d) & 255;
    }

    public static int Main(string[] args)
    {
        int a = 1;
        int b = 2;
        int c = 3;
        int d = 4;
        int acc = 0;

        for (int i = 0; i < Iterations; i++)
        {
            a = a + b + i;
            b = b + c;
            c = c + d;
            d = d + a;
            acc = acc + Step(a, b, c, d, i);
        }

        return acc & 255;
    }
}
"""

        template.Replace ("__ITERATIONS__", string guestIterations)

[<MemoryDiagnoser>]
type StackHeavyProgramBenchmarks () =
    let sourceName = "PerformanceBaseline.StackHeavy.cs"

    let nativeImpls : NativeImpls =
        {
            System_Environment = System_EnvironmentMock.Empty
        }

    let mutable image : byte array = Array.empty
    let mutable expectedExitCode : int = 0
    let mutable dotnetRuntimeDirs : ImmutableArray<string> = ImmutableArray.Empty

    [<Params(4096)>]
    member val GuestIterations : int = 4096 with get, set

    member private _.RunPawPrint () : int =
        use peImage = new MemoryStream (image)

        match
            WoofWare.PawPrint.Program.run
                NullLoggerFactory.Instance
                (Some sourceName)
                peImage
                dotnetRuntimeDirs
                nativeImpls
                []
        with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 exitCode :: _ -> exitCode
            | [] -> failwith "Expected PawPrint run to leave an int exit code, but the stack was empty"
            | head :: _ -> failwith $"Expected PawPrint run to leave an int exit code, but got %O{head}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"PawPrint threw an unhandled guest exception: %O{exn.ExceptionObject}"

    [<GlobalSetup>]
    member this.GlobalSetup () : unit =
        image <-
            GuestPrograms.stackHeavy this.GuestIterations
            |> List.singleton
            |> Roslyn.compile

        expectedExitCode <-
            match RealRuntime.executeWithRealRuntime [||] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException exn ->
                failwith $"Real runtime threw unexpectedly while establishing perf baseline oracle: %O{exn}"

        dotnetRuntimeDirs <-
            DotnetRuntime.SelectForDll (typeof<StackHeavyProgramBenchmarks>.Assembly.Location)
            |> ImmutableArray.CreateRange

        let actualExitCode = this.RunPawPrint ()

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

    [<Benchmark(Description = "Run stack-heavy guest program")>]
    member this.RunStackHeavyGuestProgram () : int =
        let actualExitCode = this.RunPawPrint ()

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

        actualExitCode

module Program =
    [<EntryPoint>]
    let main (args : string[]) : int =
        BenchmarkSwitcher.FromAssembly(typeof<StackHeavyProgramBenchmarks>.Assembly).Run args
        |> ignore

        0
