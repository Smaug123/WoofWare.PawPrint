namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// PawPrint interprets whichever CoreLib it is pointed at, and CoreLib is `#if`-split per
/// target: `System.Threading.Lock.ThreadId.InitializeForCurrentThread` calls
/// `GetUInt64OSThreadId` under `TARGET_OSX` and `TryGetUInt32OSThreadId` everywhere else, and
/// that is one example of many. Every other entry point in the repo resolves the *host's*
/// shared framework, so a macOS dev box can only ever exercise the macOS BCL while production
/// and CI run the Linux one — genuinely different guest code, previously with no way to reach
/// it locally.
///
/// These tests pin the seam that closes that gap: a pinned linux-x64 framework (Nix's
/// `dotnet-linux-framework`, surfaced as `$DOTNET_LINUX_FRAMEWORK_DIR`) placed at the head of
/// the interpreter's runtime-dir list, which binds by simple name and takes the first hit.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLinuxCoreLibFlavour =

    let private assy = typeof<RunResult>.Assembly

    let private linuxFrameworkDir : string option =
        match Environment.GetEnvironmentVariable "DOTNET_LINUX_FRAMEWORK_DIR" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned framework only exists inside the Nix devshell, so a plain `dotnet test` in a
    /// non-Nix checkout skips rather than fails. Everything these tests assert is about a
    /// foreign CoreLib flavour, so there is nothing meaningful to fall back to.
    let private requireLinuxFramework () : string =
        match linuxFrameworkDir with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_LINUX_FRAMEWORK_DIR is unset; run under `nix develop` to exercise the linux-x64 CoreLib."
            // Assert.Ignore throws, so this is unreachable; it exists to satisfy the type checker.
            failwith "unreachable: Assert.Ignore did not throw"

    let private corelibPath (frameworkDir : string) : string =
        Path.Combine (frameworkDir, "System.Private.CoreLib.dll")

    /// Every native entry point the assembly imports, by entry-point name. Whole-assembly
    /// rather than navigated to: the declaring types here (`Interop+Sys` and friends) are
    /// private nested types, and the question being asked really is "does this image import
    /// this primitive at all".
    let private nativeEntryPointNames (assembly : DumpedAssembly) : Set<string> =
        assembly.TypeDefs.Values
        |> Seq.collect (fun ty -> ty.Methods)
        |> Seq.choose (fun method -> method.NativeImport |> Option.map _.EntryPointName)
        |> Set.ofSeq

    /// Guards against `$DOTNET_LINUX_FRAMEWORK_DIR` pointing at the wrong pack — including at a
    /// macOS one, which would make every other test here quietly assert nothing. The two entry
    /// points are the opposite arms of the same `#if` in `Lock.NonNativeAot.cs`, so exactly one
    /// of them is present in any given CoreLib.
    [<Test>]
    let ``the pinned framework really is the Linux CoreLib flavour`` () : unit =
        let frameworkDir = requireLinuxFramework ()
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let imports =
            Assembly.readFile loggerFactory (corelibPath frameworkDir)
            |> nativeEntryPointNames

        imports
        |> Set.contains "SystemNative_TryGetUInt32OSThreadId"
        |> shouldEqual true

        imports |> Set.contains "SystemNative_GetUInt64OSThreadId" |> shouldEqual false

    /// Runtime dirs with the pinned linux-x64 framework first. Assembly binding takes the first
    /// directory that has a `<simple name>.dll`, so every framework assembly resolves from the
    /// pack; the host's dirs stay on the list only to bind anything the pack does not carry.
    let private runtimeDirsPreferringLinux (frameworkDir : string) : ImmutableArray<string> =
        seq {
            yield frameworkDir
            yield! DotnetRuntime.SelectForDll assy.Location
        }
        |> ImmutableArray.CreateRange

    let private runOnLinuxFramework (frameworkDir : string) (source : string) : IlMachineState * ThreadId =
        let image = Roslyn.compile [ source ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let outcome =
            Program.run
                loggerFactory
                (Some "LinuxCoreLibFlavour.cs")
                peImage
                (runtimeDirsPreferringLinux frameworkDir)
                KernelConfig.Default
                None
                []

        match outcome with
        | RunOutcome.NormalExit (terminalState, terminatingThread) -> terminalState, terminatingThread
        | other -> failwith $"Expected the guest to exit normally on the linux-x64 CoreLib, got %O{other}"

    let private exitCode (terminalState : IlMachineState) (thread : ThreadId) : int =
        match terminalState.ThreadState.[thread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 code :: _ -> code
        | [] -> failwith "Guest returned void; expected an int32 exit code"
        | other :: _ -> failwith $"Guest left %O{other} on the eval stack; expected an int32 exit code"

    let private loadedCorelibPath (terminalState : IlMachineState) : string =
        let corelibs =
            terminalState._LoadedAssemblies.DefinitionNames
            |> Seq.choose terminalState._LoadedAssemblies.TryByDefinitionName
            |> Seq.filter (fun loaded -> loaded.Name.Name = "System.Private.CoreLib")
            |> Seq.toList

        match corelibs with
        | [ corelib ] ->
            corelib.OriginalPath
            |> Option.defaultWith (fun () ->
                failwith "Loaded CoreLib has no OriginalPath; cannot tell where it came from"
            )
        | [] -> failwith "No System.Private.CoreLib was loaded"
        | many ->
            let paths = many |> List.map (fun c -> string<string option> c.OriginalPath)

            failwith
                $"""Expected exactly one loaded System.Private.CoreLib, got %d{many.Length}: %s{String.Join (", ", paths)}"""

    /// The load-bearing test: not just that we passed a directory, but that the interpreter
    /// really bound CoreLib out of it and ran a guest to completion against that image.
    /// Deliberately reaches past pure arithmetic into string and collection code, since that is
    /// where the two flavours' CoreLib IL actually differs and where a flavour-specific
    /// unimplemented primitive would surface.
    [<Test>]
    let ``a guest runs against the pinned linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System;
using System.Collections.Generic;
using System.Text;

public class Program
{
    public static int Main(string[] args)
    {
        int total = 0;
        for (int i = 0; i < 10; i++)
        {
            total += i;
        }

        if (total != 45)
        {
            return 1;
        }

        var numbers = new List<int> { 3, 1, 2 };
        numbers.Sort();
        if (numbers[0] != 1 || numbers[2] != 3)
        {
            return 2;
        }

        var builder = new StringBuilder();
        foreach (int n in numbers)
        {
            builder.Append(n);
        }

        if (builder.ToString() != "123")
        {
            return 3;
        }

        var lookup = new Dictionary<string, int> { ["answer"] = 42 };
        if (!lookup.TryGetValue("answer", out int answer) || answer != 42)
        {
            return 4;
        }

        return 0;
    }
}
"""

        let terminalState, thread = runOnLinuxFramework frameworkDir source

        exitCode terminalState thread |> shouldEqual 0
        loadedCorelibPath terminalState |> shouldEqual (corelibPath frameworkDir)
