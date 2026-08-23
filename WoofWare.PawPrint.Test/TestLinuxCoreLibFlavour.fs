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
/// and CI run the Linux one — different guest code.
///
/// These tests pin the mechanism that closes that gap: a pinned linux-x64 framework (Nix's
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
        |> Seq.choose (fun method -> method.TryNativeImport |> Option.map _.EntryPointName)
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
                (HostConfig.Default (runtimeDirsPreferringLinux frameworkDir))

        match outcome with
        | RunOutcome.NormalExit (terminalState, terminatingThread) -> terminalState, terminatingThread
        | other -> failwith $"Expected the guest to exit normally on the linux-x64 CoreLib, got %O{other}"

    let private exitCode (terminalState : IlMachineState) (thread : ThreadId) : int =
        match terminalState.ThreadState.[thread].MethodState.EvaluationStack.Values with
        | EvalStackValue.Int32 (Int32Source.Verbatim code) :: _ -> code
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

    /// Not just that we passed a directory: the interpreter
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

    /// The production path for `SystemNative_TryGetUInt32OSThreadId`, which is the whole reason
    /// the two arms of `Lock.NonNativeAot.cs`'s `#if` are worth distinguishing.
    ///
    /// `sourcesPure/SystemNativeOSThreadId.cs` covers the handler through a hand-rolled
    /// `DllImport`, which reaches it on any host but proves nothing about how CoreLib gets
    /// there. This reaches it the way real guest code does — `System.Threading.Lock` ->
    /// `Lock.ThreadId.InitializeForCurrentThread` -> `Interop.Sys.TryGetUInt32OSThreadId` — and
    /// only the Linux flavour takes that arm, so on a macOS host this is the only place the
    /// 32-bit entry point is exercised in anger at all.
    ///
    /// Asserting mutual exclusion would prove more, but PawPrint's scheduler makes an
    /// uncontended `Lock` the only shape reachable without a great deal more machinery. What is
    /// asserted is the property `InitializeForCurrentThread` actually depends on: that the id it
    /// stored is usable as an identity, so a recursive acquire is recognised as the *same*
    /// thread and a nested `Exit` does not throw `SynchronizationLockException`.
    [<Test>]
    let ``System.Threading.Lock reaches TryGetUInt32OSThreadId on the linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System.Threading;

public class Program
{
    public static int Main(string[] args)
    {
        Lock gate = new Lock();

        // Enter/Exit drives ThreadId.InitializeForCurrentThread, which on this
        // CoreLib flavour is the TryGetUInt32OSThreadId arm of the #if.
        gate.Enter();
        if (!gate.IsHeldByCurrentThread)
        {
            return 1;
        }

        // Recursive acquire: only recognised as re-entry if the id minted for
        // this thread compares equal to itself, i.e. is a stable identity.
        gate.Enter();
        if (!gate.IsHeldByCurrentThread)
        {
            return 2;
        }

        gate.Exit();
        if (!gate.IsHeldByCurrentThread)
        {
            return 3;
        }

        gate.Exit();
        if (gate.IsHeldByCurrentThread)
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

    /// `BitOperations.LeadingZeroCount`'s uint32 and uint64 overloads are modelled as arms, but
    /// its `(nuint)` overload is allowlisted instead, so on that one PawPrint runs CoreLib's own
    /// IL. That IL is exactly what differs between flavours: the `IsSupported` guards ahead of
    /// the forwarding call are folded to constants when CoreLib was built for another
    /// architecture, and are live calls when it was built for this one. A macOS/arm64 run
    /// therefore cannot stand in for the x64 flavour that CI and production interpret.
    ///
    /// The same applies to the six `[Intrinsic]`-marked `IBinaryInteger<TSelf>.LeadingZeroCount`
    /// wrappers, whose bodies are likewise executed rather than modelled.
    [<Test>]
    let ``LeadingZeroCount runs against the pinned linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System;
using System.Numerics;

public class Program
{
    public static int Main(string[] args)
    {
        // The two modelled widths.
        if (BitOperations.LeadingZeroCount(0u) != 32) return 1;
        if (BitOperations.LeadingZeroCount(1u) != 31) return 2;
        if (BitOperations.LeadingZeroCount(0ul) != 64) return 3;
        if (BitOperations.LeadingZeroCount(1ul) != 63) return 4;

        // The allowlisted forwarder, whose CoreLib IL is what this test exists to run.
        int width = IntPtr.Size * 8;
        if (BitOperations.LeadingZeroCount((nuint)0) != width) return 5;
        if (BitOperations.LeadingZeroCount((nuint)1) != width - 1) return 6;
        if (BitOperations.LeadingZeroCount(nuint.MaxValue) != 0) return 7;
        if (BitOperations.LeadingZeroCount(default(nuint)) != width) return 8;

        // The allowlisted IBinaryInteger wrappers.
        if (uint.LeadingZeroCount(1u) != 31u) return 9;
        if (ulong.LeadingZeroCount(1ul) != 63ul) return 10;
        if (nuint.LeadingZeroCount((nuint)1) != (nuint)(width - 1)) return 11;
        if (int.LeadingZeroCount(-1) != 0) return 12;
        if (long.LeadingZeroCount(-1L) != 0L) return 13;
        if (nint.LeadingZeroCount((nint)(-1)) != (nint)0) return 14;

        return 0;
    }
}
"""

        let terminalState, thread = runOnLinuxFramework frameworkDir source

        exitCode terminalState thread |> shouldEqual 0
        loadedCorelibPath terminalState |> shouldEqual (corelibPath frameworkDir)

    /// The flavour risk is larger for `TrailingZeroCount` than for its sibling: only the uint32
    /// overload is modelled, so every other overload's CoreLib body is *executed*, including the
    /// uint64 one whose fallback splits the value into halves. Those bodies are precisely where
    /// the flavours diverge — `Bmi1`/`X86Base` guards are folded to constant false in an arm64
    /// build but are live calls in the x64 build that CI and production interpret, and the arm64
    /// `ArmBase` guards are the other way round. A macOS/arm64 run exercises only one of those
    /// shapes, so this case runs the other.
    [<Test>]
    let ``TrailingZeroCount runs against the pinned linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System;
using System.Numerics;

public class Program
{
    public static int Main(string[] args)
    {
        // The modelled width.
        if (BitOperations.TrailingZeroCount(0u) != 32) return 1;
        if (BitOperations.TrailingZeroCount(0x80000000u) != 31) return 2;

        // The executed uint64 body, including the half-splitting fallback: a value whose low
        // 32 bits are zero is the branch that recurses into the modelled uint32 overload.
        if (BitOperations.TrailingZeroCount(0ul) != 64) return 3;
        if (BitOperations.TrailingZeroCount(1ul) != 0) return 4;
        if (BitOperations.TrailingZeroCount(0x100000000ul) != 32) return 5;
        if (BitOperations.TrailingZeroCount(0x8000000000000000ul) != 63) return 6;

        // The executed signed and native-width forwarders.
        int width = IntPtr.Size * 8;
        if (BitOperations.TrailingZeroCount(0L) != 64) return 7;
        if (BitOperations.TrailingZeroCount(long.MinValue) != 63) return 8;
        if (BitOperations.TrailingZeroCount((nuint)0) != width) return 9;
        if (BitOperations.TrailingZeroCount((nuint)256) != 8) return 10;
        if (BitOperations.TrailingZeroCount(default(nuint)) != width) return 11;
        if (BitOperations.TrailingZeroCount((nint)(-2)) != 1) return 12;

        // The executed IBinaryInteger wrappers.
        if (uint.TrailingZeroCount(0x80000000u) != 31u) return 13;
        if (ulong.TrailingZeroCount(0x100000000ul) != 32ul) return 14;
        if (nuint.TrailingZeroCount((nuint)256) != (nuint)8) return 15;
        if (int.TrailingZeroCount(int.MinValue) != 31) return 16;
        if (long.TrailingZeroCount(long.MinValue) != 63L) return 17;
        if (nint.TrailingZeroCount((nint)(-2)) != (nint)1) return 18;

        return 0;
    }
}
"""

        let terminalState, thread = runOnLinuxFramework frameworkDir source

        exitCode terminalState thread |> shouldEqual 0
        loadedCorelibPath terminalState |> shouldEqual (corelibPath frameworkDir)

    /// `BitOperations.PopCount` is the strongest flavour case of the three: no width of it is
    /// modelled at all, so every overload's CoreLib body is *executed*. Those bodies are exactly
    /// where the flavours diverge — the x64 build calls `Popcnt.IsSupported` for real and has
    /// folded the `AdvSimd.Arm64` guard to a constant false, while the arm64 build a macOS dev
    /// box interprets does the reverse and additionally carries the live `Vector64`/`AdvSimd`
    /// calls behind that guard. Both must reach the same software fallback.
    ///
    /// The same applies to the six `[Intrinsic]`-marked `IBinaryInteger<TSelf>.PopCount`
    /// wrappers, whose bodies are likewise executed rather than modelled.
    [<Test>]
    let ``PopCount runs against the pinned linux-x64 CoreLib`` () : unit =
        let frameworkDir = requireLinuxFramework ()

        let source =
            """
using System;
using System.Numerics;

public class Program
{
    public static int Main(string[] args)
    {
        // The executed uint32 body: the extremes, plus a pattern where the fallback's
        // byte-wise partial sums are all distinct.
        if (BitOperations.PopCount(0u) != 0) return 1;
        if (BitOperations.PopCount(uint.MaxValue) != 32) return 2;
        if (BitOperations.PopCount(0x55555555u) != 16) return 3;
        if (BitOperations.PopCount(0x01234567u) != 12) return 4;

        // The executed uint64 body. On a 64-bit build this is its own software fallback
        // (multiply by 0x0101010101010101, shift right by 56), not a split into halves.
        if (BitOperations.PopCount(0ul) != 0) return 5;
        if (BitOperations.PopCount(ulong.MaxValue) != 64) return 6;
        if (BitOperations.PopCount(0xFFFFFFFF00000000ul) != 32) return 7;
        if (BitOperations.PopCount(0x0123456789ABCDEFul) != 32) return 8;

        // The executed native-width forwarder.
        int width = IntPtr.Size * 8;
        if (BitOperations.PopCount((nuint)0) != 0) return 9;
        if (BitOperations.PopCount(nuint.MaxValue) != width) return 10;
        if (BitOperations.PopCount(default(nuint)) != 0) return 11;
        if (BitOperations.PopCount((nuint)255) != 8) return 12;

        // The executed IBinaryInteger wrappers.
        if (uint.PopCount(uint.MaxValue) != 32u) return 13;
        if (ulong.PopCount(ulong.MaxValue) != 64ul) return 14;
        if (nuint.PopCount(nuint.MaxValue) != (nuint)width) return 15;
        if (int.PopCount(-1) != 32) return 16;
        if (long.PopCount(-1L) != 64L) return 17;
        if (nint.PopCount((nint)(-1)) != (nint)width) return 18;

        return 0;
    }
}
"""

        let terminalState, thread = runOnLinuxFramework frameworkDir source

        exitCode terminalState thread |> shouldEqual 0
        loadedCorelibPath terminalState |> shouldEqual (corelibPath frameworkDir)
