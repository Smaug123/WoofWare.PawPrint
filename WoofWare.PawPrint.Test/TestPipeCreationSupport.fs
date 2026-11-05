namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SystemNative_IsAtomicNonInheritablePipeCreationSupported`, .NET 11's report of whether its
/// shim was built with `pipe2(2)`. It is a compile-time constant of the shim, so it differs between
/// the two Unixes PawPrint models, and a `sourcesPure` guest could assert no more of it than "0 or 1,
/// and stable", which a hardcoded constant satisfies. As in `TestPlatformSocketSupport`, the flavour
/// dependence is pinned here against PawPrint's own kernel configuration, and each flavour's value
/// against .NET 11's real shim on this host.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeCreationSupport =

    [<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
    type private IsAtomicNonInheritablePipeCreationSupported = delegate of unit -> int32

    /// A raw `[DllImport]` stub rather than a route through the BCL: the managed caller,
    /// `Process.Start`'s `PrepareStandardHandles`, latches the answer in a static and branches on
    /// it only to decide whether to take a lock, which no guest can observe.
    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_IsAtomicNonInheritablePipeCreationSupported")]
    static extern int IsAtomicNonInheritablePipeCreationSupported();

    static int Main(string[] args)
    {
        int first = IsAtomicNonInheritablePipeCreationSupported();

        // A compile-time constant of the shim, so every call answers identically. Reported as a
        // distinct exit code from either legitimate answer.
        if (IsAtomicNonInheritablePipeCreationSupported() != first) return 2;

        return first;
    }
}
"""

    /// The exit code a cleanly terminated guest latched.
    let private exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, _, _)
        | RunOutcome.ProcessExit (state, _, _) -> state.LatchedExitCode
        | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

    let private runOn (name : string) (platform : SimulatedUnixPlatform) : int =
        let image = Roslyn.compile [ guest ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes = FrameworkUnderTest.runtimeDirsFor platform

        use peImage = new MemoryStream (image)

        BoundedRun.run
            loggerFactory
            name
            (Some name)
            peImage
            { HostConfig.Default dotnetRuntimes with
                Guest =
                    { GuestConfig.Default dotnetRuntimes with
                        Kernel =
                            { KernelConfig.Default with
                                UnixPlatform = platform
                            }
                    }
            }
        |> ExpectRun.ended
        |> exitCodeOf

    /// Both columns, from the interpreter rather than from the model directly, so that the
    /// handler's read of `Kernel.UnixPlatform` is what is tested: a handler hardcoding either
    /// constant fails one of the two.
    [<Test>]
    let ``the answer follows the simulated flavour`` () : unit =
        runOn "AtomicPipeCreationLinux.cs" SimulatedUnixPlatform.linuxX64
        |> shouldEqual 1

        runOn "AtomicPipeCreationDarwin.cs" SimulatedUnixPlatform.macOsArm64
        |> shouldEqual 0

    /// The per-flavour constants against .NET 11's real shim: a macOS dev box checks the Darwin
    /// column and a Linux CI runner the Linux one. .NET 10's shim has no such export.
    [<Test>]
    let ``the model agrees with .NET 11's own shim on this host`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let directory =
                FrameworkUnderTest.requireSharedFrameworkDirectoryOf EmulatedRuntime.Net11

            let file =
                if OperatingSystem.IsMacOS () then
                    "libSystem.Native.dylib"
                else
                    "libSystem.Native.so"

            let library = NativeLibrary.Load (Path.Combine (directory, file))

            let native =
                NativeLibrary.GetExport (library, "SystemNative_IsAtomicNonInheritablePipeCreationSupported")
                |> Marshal.GetDelegateForFunctionPointer<IsAtomicNonInheritablePipeCreationSupported>

            let measured = native.Invoke ()

            // The upstream body (pal_io.c) is `#if HAVE_PIPE2 return 1; #else return 0;`, so
            // anything else means the symbol resolved to something other than the function modelled.
            if measured <> 0 && measured <> 1 then
                failwith
                    $"SystemNative_IsAtomicNonInheritablePipeCreationSupported returned %d{measured} on this host, but upstream can only return 0 or 1."

            (measured = 1)
            |> shouldEqual (PipeFlagsPal.atomicNonInheritableCreation (HostPlatform.platformOf flavour))
        )
