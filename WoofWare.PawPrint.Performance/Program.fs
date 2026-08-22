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

/// A deliberately in-process runner, and NOT the differential oracle.
///
/// `WoofWare.PawPrint.Test`'s `RealRuntime` runs guests out of process, because in-process the
/// guest shares CoreCLR's process-global latched exit code with its host, and `Environment.Exit`
/// or an escaped exception terminates the host outright. None of that matters here: this is a
/// BenchmarkDotNet harness whose real-runtime arm exists to give PawPrint's timings something to
/// be compared against, no assertion depends on the exit code, and its guests are fixed and
/// well-behaved. Spawning a process per iteration would instead add roughly 27ms of runtime
/// startup to every measurement, which is the same order as the guest work being measured.
///
/// Do not promote this into a correctness oracle. If a comparison ever needs to be asserted on
/// here, use the out-of-process runner rather than relaxing this one.
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

    /// A guest whose calls pass reference-typed arguments, so that per-call work proportional to
    /// the number of object arguments is visible in a measurement. `stackHeavy` passes only ints
    /// and cannot show it.
    let referenceArgHeavy (guestIterations : int) : string =
        let template =
            """
public sealed class Node
{
    public int Value;
    public Node (int value) { Value = value; }
}

public static class Program
{
    const int Iterations = __ITERATIONS__;

    static int Combine (Node a, Node b, Node c, string tag)
    {
        return (a.Value + b.Value + c.Value + tag.Length) & 255;
    }

    public static int Main (string[] args)
    {
        Node x = new Node (1);
        Node y = new Node (2);
        Node z = new Node (3);
        string tag = "tag";
        int acc = 0;

        for (int i = 0; i < Iterations; i++)
        {
            x.Value = (x.Value + i) & 255;
            y.Value = (y.Value + x.Value) & 255;
            z.Value = (z.Value + y.Value) & 255;
            acc = acc + Combine (x, y, z, tag);
        }

        return acc & 255;
    }
}
"""

        template.Replace ("__ITERATIONS__", string guestIterations)

    /// A guest whose inner loop is virtual dispatch: one call site, four receiver types, so what is
    /// measured is resolving a `callvirt` rather than executing the body it lands on. `stackHeavy` and
    /// `referenceArgHeavy` call only non-virtual methods, so neither shows this cost at all.
    ///
    /// The receivers rotate deliberately. A resolver that memoised its answer per call site would
    /// look fast on a monomorphic loop and slow here, which is the distinction worth being able to
    /// see, and the depth means a whole-chain walk costs more than a one-level one.
    ///
    /// The checksum is multiplicative and reduced mod 251 because `Harness.setUp` compares it against
    /// the real runtime, so it is this guest's only guard against benchmarking a *wrong* dispatch.
    /// A plain sum masked to a byte cannot be that guard: over an iteration count that is a multiple
    /// of four the receiver contributions sum to a multiple of 256, so answering every call with
    /// `Base.M` -- or with `D3.M`, or with the rotation offset by one -- produces the identical
    /// checksum. Computed over those five wrong dispatch models plus the correct one, this form
    /// gives six distinct values.
    let virtualDispatchHeavy (guestIterations : int) : string =
        let template =
            """
public class Base
{
    public virtual int M (int x)
    {
        return x + 1;
    }
}

public class D1 : Base
{
    public override int M (int x)
    {
        return x + 2;
    }
}

public class D2 : D1
{
    public override int M (int x)
    {
        return x + 3;
    }
}

public class D3 : D2
{
    public override int M (int x)
    {
        return x + 4;
    }
}

public static class Program
{
    const int Iterations = __ITERATIONS__;

    public static int Main (string[] args)
    {
        Base[] receivers = new Base[] { new Base (), new D1 (), new D2 (), new D3 () };
        int acc = 0;

        for (int i = 0; i < Iterations; i++)
        {
            Base r = receivers[i & 3];
            acc = (acc * 31 + r.M (i)) & 0xFFFFFF;
        }

        return acc % 251;
    }
}
"""

        template.Replace ("__ITERATIONS__", string guestIterations)

[<RequireQualifiedAccess>]
module private Harness =
    /// Names this assembly, whose location `DotnetRuntime.SelectForDll` needs to pick the shared
    /// framework the guests resolve against.
    type private Marker = class end

    /// Interpret `image` to completion and return the exit code the guest left on the stack.
    let runPawPrint (sourceName : string) (image : byte array) (dotnetRuntimeDirs : ImmutableArray<string>) : int =
        use peImage = new MemoryStream (image)

        match
            WoofWare.PawPrint.Program.run
                NullLoggerFactory.Instance
                (Some sourceName)
                peImage
                (HostConfig.Default dotnetRuntimeDirs)
        with
        | RunOutcome.NormalExit (terminalState, terminatingThread)
        | RunOutcome.ProcessExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode
            | [] -> failwith "Expected PawPrint run to leave an int exit code, but the stack was empty"
            | head :: _ -> failwith $"Expected PawPrint run to leave an int exit code, but got %O{head}"
        | RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"PawPrint guest aborted (%O{fatal.Code}): %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"PawPrint guest was terminated by POSIX signal %O{signal} during benchmark"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"PawPrint threw an unhandled guest exception: %O{exn.ExceptionObject}"

    /// Compile `source`, establish the expected exit code on real .NET, and check PawPrint agrees
    /// before any timing is taken.
    let setUp (sourceName : string) (source : string) : byte array * int * ImmutableArray<string> =
        let image = Roslyn.compile [ source ]

        let expectedExitCode =
            match RealRuntime.executeWithRealRuntime [||] image with
            | RealRuntimeResult.NormalExit exitCode -> exitCode
            | RealRuntimeResult.UnhandledException exn ->
                failwith $"Real runtime threw unexpectedly while establishing perf baseline oracle: %O{exn}"

        let dotnetRuntimeDirs =
            DotnetRuntime.SelectForDll (typeof<Marker>.Assembly.Location)
            |> ImmutableArray.CreateRange

        let actualExitCode = runPawPrint sourceName image dotnetRuntimeDirs

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

        image, expectedExitCode, dotnetRuntimeDirs

[<MemoryDiagnoser>]
type StackHeavyProgramBenchmarks () =
    let sourceName = "PerformanceBaseline.StackHeavy.cs"

    let mutable image : byte array = Array.empty
    let mutable expectedExitCode : int = 0
    let mutable dotnetRuntimeDirs : ImmutableArray<string> = ImmutableArray.Empty

    [<Params(4096)>]
    member val GuestIterations : int = 4096 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup () : unit =
        let img, expected, dirs =
            Harness.setUp sourceName (GuestPrograms.stackHeavy this.GuestIterations)

        image <- img
        expectedExitCode <- expected
        dotnetRuntimeDirs <- dirs

    [<Benchmark(Description = "Run stack-heavy guest program")>]
    member _.RunStackHeavyGuestProgram () : int =
        let actualExitCode = Harness.runPawPrint sourceName image dotnetRuntimeDirs

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

        actualExitCode

/// Companion to `StackHeavyProgramBenchmarks`, whose guest passes only ints. Per-call work that
/// scales with the number of *reference-typed* arguments is invisible there, so this guest passes
/// objects and a string on every call.
[<MemoryDiagnoser>]
type ReferenceArgProgramBenchmarks () =
    let sourceName = "PerformanceBaseline.ReferenceArgHeavy.cs"

    let mutable image : byte array = Array.empty
    let mutable expectedExitCode : int = 0
    let mutable dotnetRuntimeDirs : ImmutableArray<string> = ImmutableArray.Empty

    [<Params(4096)>]
    member val GuestIterations : int = 4096 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup () : unit =
        let img, expected, dirs =
            Harness.setUp sourceName (GuestPrograms.referenceArgHeavy this.GuestIterations)

        image <- img
        expectedExitCode <- expected
        dotnetRuntimeDirs <- dirs

    [<Benchmark(Description = "Run reference-argument-heavy guest program")>]
    member _.RunReferenceArgGuestProgram () : int =
        let actualExitCode = Harness.runPawPrint sourceName image dotnetRuntimeDirs

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

        actualExitCode


/// Companion to the two above, whose guests call only non-virtual methods. Every `callvirt` is
/// resolved from scratch today, so the cost of resolution is invisible in their timings.
[<MemoryDiagnoser>]
type VirtualDispatchProgramBenchmarks () =
    let sourceName = "PerformanceBaseline.VirtualDispatchHeavy.cs"

    let mutable image : byte array = Array.empty
    let mutable expectedExitCode : int = 0
    let mutable dotnetRuntimeDirs : ImmutableArray<string> = ImmutableArray.Empty

    [<Params(4096)>]
    member val GuestIterations : int = 4096 with get, set

    [<GlobalSetup>]
    member this.GlobalSetup () : unit =
        let img, expected, dirs =
            Harness.setUp sourceName (GuestPrograms.virtualDispatchHeavy this.GuestIterations)

        image <- img
        expectedExitCode <- expected
        dotnetRuntimeDirs <- dirs

    [<Benchmark(Description = "Run virtual-dispatch-heavy guest program")>]
    member _.RunVirtualDispatchGuestProgram () : int =
        let actualExitCode = Harness.runPawPrint sourceName image dotnetRuntimeDirs

        if actualExitCode <> expectedExitCode then
            failwith $"PawPrint returned %d{actualExitCode}, but real runtime returned %d{expectedExitCode}"

        actualExitCode


module Program =
    /// `--profile <stack|refarg|virt> <guestIterations> <reps>`: run one benchmark guest in *this*
    /// process, with no BenchmarkDotNet around it, and report milliseconds per repetition.
    ///
    /// It exists because BenchmarkDotNet answers "how long" but not "doing what". Attaching a
    /// sampling or allocation profiler to a `BenchmarkSwitcher` run means attaching it to the host,
    /// which then spawns a per-benchmark child process that does the actual work; this entry point
    /// is the guest work with nothing in front of it, so `dotnet-trace collect -- dotnet
    /// WoofWare.PawPrint.Performance.dll --profile ...` profiles the interpreter directly.
    ///
    /// Varying `guestIterations` also separates the two costs the fixed-`Params` benchmarks report
    /// as one number: a run at zero iterations is startup alone (loading CoreLib, pumping its
    /// `.cctor`s, reaching and returning from `Main`), and the slope above it is steady-state
    /// interpretation. On this machine that split is roughly 71ms fixed against ~65µs per guest
    /// loop iteration, so at the benchmarks' 4096 iterations startup is about a fifth of the total
    /// — worth knowing before concluding that a change to the instruction loop did nothing.
    ///
    /// The exit-code check `Harness.setUp` performs still runs, so a profiling run that silently
    /// interpreted the guest wrongly fails rather than reporting a fast wrong number.
    let private profile (guest : string) (guestIterations : int) (reps : int) : int =
        let sourceName, source =
            match guest with
            | "stack" -> "Profile.StackHeavy.cs", GuestPrograms.stackHeavy guestIterations
            | "refarg" -> "Profile.ReferenceArgHeavy.cs", GuestPrograms.referenceArgHeavy guestIterations
            | "virt" -> "Profile.VirtualDispatchHeavy.cs", GuestPrograms.virtualDispatchHeavy guestIterations
            | other -> failwith $"Unknown profile guest %s{other}; expected stack, refarg or virt"

        // Rejected rather than tolerated: with `reps` at zero the loop below runs nothing and the
        // division reports `Infinity`, which is a successful exit carrying a meaningless number —
        // exactly the shape of result a profiling run must never produce quietly.
        if reps < 1 then
            failwith $"--profile needs at least one repetition to time; got %d{reps}"

        let image, expected, dirs = Harness.setUp sourceName source
        eprintfn $"[profile] warm-up done, expected exit code %d{expected}; running %d{reps} reps"

        let mutable acc = 0
        let sw = Diagnostics.Stopwatch.StartNew ()

        for _ in 1..reps do
            acc <- acc + Harness.runPawPrint sourceName image dirs

        sw.Stop ()

        let perRep = sw.Elapsed.TotalMilliseconds / float reps

        eprintfn
            $"[profile] guest=%s{guest} iters=%d{guestIterations} reps=%d{reps} perRep=%.2f{perRep}ms checksum=%d{acc}"

        0

    [<EntryPoint>]
    let main (args : string[]) : int =
        match args with
        | [| "--profile" ; guest ; guestIterations ; reps |] -> profile guest (int guestIterations) (int reps)
        | _ ->

        BenchmarkSwitcher.FromAssembly(typeof<StackHeavyProgramBenchmarks>.Assembly).Run args
        |> ignore

        0
