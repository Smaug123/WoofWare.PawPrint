namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// PawPrint's virtual CPU is `HardwareIntrinsicsProfile.ScalarOnly`: every capability query
/// answers false, and every hardware instruction throws `PlatformNotSupportedException`. A CoreLib
/// carries placeholder IL for its own architecture's instruction sets and a `false`/throwing twin
/// for every other architecture's, so each test runs on the framework under test and on the
/// pinned linux-x64 CoreLib: between them, both the placeholders and the twins of the X86 and Arm
/// classes are exercised whatever the host.
///
/// These are not differential tests. The real runtime answers from the host's CPU, which is
/// exactly the dependence the profile exists to remove.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestHardwareIntrinsicsProfile =
    let private exitCodeOfRunOutcome (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.UndefinedValueObserved (_, _, observation) ->
            failwith $"guest used an undefined value: %O{observation}"
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
        | RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"PawPrint guest aborted (%O{fatal.Code}): %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"PawPrint guest was terminated by POSIX signal %O{signal}"
        | RunOutcome.GuestUnhandledException (finalState, _, exn) ->
            failwith
                $"PawPrint threw an unexpected guest exception:\n%s{UnhandledExceptionReport.describe finalState exn}"

    let flavours : TestCaseData list =
        [
            TestCaseData("framework under test").SetArgDisplayNames "framework under test"
            TestCaseData("linux-x64").SetArgDisplayNames "pinned linux-x64 CoreLib"
        ]

    let private runtimeDirs (flavour : string) : ImmutableArray<string> =
        match flavour with
        | "framework under test" -> FrameworkUnderTest.runtimeDirs ()
        | "linux-x64" ->
            LinuxCoreLibFlavour.requireLinuxFramework ()
            |> LinuxCoreLibFlavour.runtimeDirsPreferringLinux
        | other -> failwith $"unknown flavour %s{other}"

    let private runSource (flavour : string) (sourceFileName : string) (source : string) : int =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceFileName ]

        use _loggerFactoryResource = loggerFactory

        use peImage = new MemoryStream (image)

        try
            Program.run loggerFactory (Some sourceFileName) peImage (HostConfig.Default (runtimeDirs flavour))
            |> exitCodeOfRunOutcome
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    [<Test>]
    let ``Scalar-only profile answers every capability query false`` () : unit =
        HardwareIntrinsicsProfile.ScalarOnly.IsSupported |> shouldEqual Set.empty

        HardwareIntrinsicsProfile.ScalarOnly.IsHardwareAccelerated
        |> shouldEqual Set.empty

    [<TestCaseSource(nameof flavours)>]
    let ``Default virtual hardware profile reports vector acceleration unavailable`` (flavour : string) : unit =
        let source =
            """
using System.Runtime.Intrinsics;

class Program
{
    static int Main(string[] args)
    {
        if (Vector64.IsHardwareAccelerated)
        {
            return 1;
        }

        if (Vector128.IsHardwareAccelerated)
        {
            return 2;
        }

        if (Vector256.IsHardwareAccelerated)
        {
            return 3;
        }

        if (Vector512.IsHardwareAccelerated)
        {
            return 4;
        }

        return 0;
    }
}
"""

        runSource flavour "HardwareIntrinsicsProfile.cs" source |> shouldEqual 0

    [<TestCaseSource(nameof flavours)>]
    let ``Default virtual hardware profile reports System.Numerics.Vector unavailable`` (flavour : string) : unit =
        let source =
            """
using System.Numerics;

class Program
{
    static int Main(string[] args)
    {
        return Vector.IsHardwareAccelerated ? 1 : 0;
    }
}
"""

        runSource flavour "NumericsVectorIsHardwareAccelerated.cs" source
        |> shouldEqual 0

    [<TestCaseSource(nameof flavours)>]
    let ``Scalar-only profile reports every instruction set unavailable`` (flavour : string) : unit =
        let source =
            """
using System.Runtime.Intrinsics.Arm;
using System.Runtime.Intrinsics.X86;

class Program
{
    static int Main(string[] args)
    {
        if (ArmBase.IsSupported) return 1;
        if (ArmBase.Arm64.IsSupported) return 2;
        if (AdvSimd.IsSupported) return 3;
        if (Rdm.IsSupported) return 4;
        if (X86Base.IsSupported) return 5;
        if (X86Base.X64.IsSupported) return 6;
        if (Ssse3.IsSupported) return 7;
        if (Sse41.IsSupported) return 8;
        if (Avx512F.VL.IsSupported) return 9;
        return 0;
    }
}
"""

        runSource flavour "IsaIsSupported.cs" source |> shouldEqual 0

    [<TestCaseSource(nameof flavours)>]
    let ``Scalar-only profile throws PlatformNotSupportedException from every hardware instruction``
        (flavour : string)
        : unit
        =
        // Each instruction is reached both by a direct call and through a delegate: CoreCLR's JIT
        // expands the placeholder into the throw in either case, because the call through the
        // delegate compiles the method's own body, whose self-call is the must-expand site.
        let source =
            """
using System;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.Arm;
using System.Runtime.Intrinsics.X86;

class Program
{
    static bool Throws(Action action)
    {
        string expected = new PlatformNotSupportedException().Message;
        try
        {
            action();
            return false;
        }
        catch (PlatformNotSupportedException e)
        {
            return e.Message == expected;
        }
    }

    static int Main(string[] args)
    {
        if (!Throws(() => X86Base.Pause())) return 1;
        if (!Throws(X86Base.Pause)) return 2;
        if (!Throws(() => ArmBase.Yield())) return 3;
        if (!Throws(ArmBase.Yield)) return 4;
        if (!Throws(() => Sse2.Add(default(Vector128<int>), default(Vector128<int>)))) return 5;
        if (!Throws(() => AdvSimd.Add(default(Vector128<int>), default(Vector128<int>)))) return 6;
        if (!Throws(() => Sse2.X64.ConvertToInt64(default(Vector128<double>)))) return 7;
        return 0;
    }
}
"""

        runSource flavour "IsaInstructionThrows.cs" source |> shouldEqual 0

    /// Only a placeholder's call to itself is expanded; the rest of its IL runs. On the linux-x64
    /// CoreLib, `Avx2.GatherVector128` checks its scale before calling itself, so on a CPU
    /// without AVX2 an invalid scale raises `ArgumentOutOfRangeException` from that check and only
    /// a valid one reaches the self-call and `PlatformNotSupportedException`. (Other
    /// architectures' CoreLibs carry a twin that throws `PlatformNotSupportedException` first.)
    [<Test>]
    let ``Scalar-only profile runs a placeholder's own IL up to its self-call`` () : unit =
        let source =
            """
using System;
using System.Runtime.Intrinsics;
using System.Runtime.Intrinsics.X86;

unsafe class Program
{
    static int Main(string[] args)
    {
        int[] data = new int[4];
        fixed (int* p = data)
        {
            try
            {
                Avx2.GatherVector128(p, default(Vector128<int>), 3);
                return 1;
            }
            catch (ArgumentOutOfRangeException)
            {
            }
            catch (PlatformNotSupportedException)
            {
                return 2;
            }

            try
            {
                Avx2.GatherVector128(p, default(Vector128<int>), 4);
                return 3;
            }
            catch (PlatformNotSupportedException)
            {
            }
        }

        return 0;
    }
}
"""

        runSource "linux-x64" "GatherScaleCheck.cs" source |> shouldEqual 0

    /// CoreCLR's JIT compiles an instruction the CPU lacks to a call to
    /// `ThrowHelpers.ThrowPlatformNotSupportedException`, so the exception's captured frames are
    /// that helper, the instruction's method, then its caller. The helper's class is
    /// `[StackTraceHidden]`, which keeps it out of the formatted `StackTrace` but not out of
    /// `new StackTrace(e)`. Measured on real .NET 10 on osx-arm64 with
    /// `DOTNET_EnableHWIntrinsic=0`, for `ArmBase.Yield`, called directly and through a delegate.
    /// PawPrint does not yet hide `[StackTraceHidden]` frames from the formatted trace
    /// (`sourcesPure/ExceptionDispatchInfoThrowPreservesTrace.cs`), so only the captured frames
    /// are compared here.
    [<Test>]
    let ``Scalar-only profile raises PlatformNotSupportedException from CoreCLR's throw helper`` () : unit =
        let source =
            """
using System;
using System.Diagnostics;
using System.Runtime.Intrinsics.X86;

class Program
{
    static int Main(string[] args)
    {
        try
        {
            X86Base.Pause();
            return 1;
        }
        catch (PlatformNotSupportedException e)
        {
            var trace = new StackTrace(e);
            if (trace.FrameCount != 3) return 2;
            if (trace.GetFrame(0).GetMethod().Name != "ThrowPlatformNotSupportedException") return 3;
            if (trace.GetFrame(0).GetMethod().DeclaringType.Name != "ThrowHelpers") return 4;
            if (trace.GetFrame(1).GetMethod().Name != "Pause") return 5;
            if (trace.GetFrame(2).GetMethod().Name != "Main") return 6;
            if (!e.StackTrace.Contains("X86Base.Pause")) return 7;
            return 0;
        }
    }
}
"""

        runSource "linux-x64" "PlatformNotSupportedFrames.cs" source |> shouldEqual 0
