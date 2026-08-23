namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `SystemNative_PlatformSupportsDualModeIPv4PacketInfo`, which is a compile-time
/// constant of the native shim and differs between the two Unixes PawPrint models.
///
/// Not a `sourcesPure` differential case, unlike its sibling
/// `sourcesPure/SystemNativeGetMaximumAddressSize.cs`: that entry point's answer
/// (128) is the same on every platform, so a guest may assert it exactly and have
/// both runtimes agree. This one diverges *by design* — PawPrint impersonates a
/// Linux kernel by default and answers 1, while the real half of a pure case runs
/// on the host, which answers 0 on a macOS dev box. The only claims a pure guest
/// could make are "the result is 0 or 1" and "it is stable across calls", both of
/// which a handler returning a hardcoded constant satisfies. So the flavour
/// dependence is pinned against PawPrint's own kernel configuration here, and the
/// per-flavour values are pinned against a real shim by
/// `the model agrees with this host's own shim` below.
///
/// What that split leaves uncovered is the real side's own `[DllImport]` binding:
/// a differential guest run with `UnixPlatform` chosen to match the host would be
/// Darwin-vs-Darwin locally and Linux-vs-Linux in CI, and so could compare exit
/// codes. Not done, because `sourcesPure/SystemNativeGetMaximumAddressSize.cs`
/// already resolves a `libSystem.Native` symbol from a guest on the real runtime,
/// so that binding is not in question.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPlatformSocketSupport =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// The real export, in the shim this test host is running against. This is
    /// the fact the model claims to reproduce — not a proxy for it — because the
    /// upstream function body is nothing but the `#if` this test measures.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_PlatformSupportsDualModeIPv4PacketInfo")>]
    extern int private hostPlatformSupportsDualModeIPv4PacketInfo()

    /// A raw `[DllImport]` stub rather than a route through the BCL. The only
    /// managed caller is `System.Net.Sockets.SocketPal`'s class initialiser, and
    /// `SocketPal` is internal to that assembly, so no guest can name it: the
    /// only way to run its cctor is to construct a `Socket`. Measured, a guest that
    /// does now gets past the cctor and stops at
    /// `SystemNative_CreateSocketEventBuffer`, which is unimplemented — so a
    /// socket-constructing guest cannot be the vehicle for this entry point either.
    let private guest : string =
        """
using System;
using System.Runtime.InteropServices;

class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PlatformSupportsDualModeIPv4PacketInfo")]
    static extern int PlatformSupportsDualModeIPv4PacketInfo();

    static int Main(string[] args)
    {
        int first = PlatformSupportsDualModeIPv4PacketInfo();

        // A compile-time constant of the shim, so every call answers
        // identically. Reported as a distinct exit code from either legitimate
        // answer, so an implementation reading mutable state cannot be mistaken
        // for one of the two platforms.
        if (PlatformSupportsDualModeIPv4PacketInfo() != first) return 2;

        return first;
    }
}
"""

    /// The exit code a terminated guest left on its terminating thread's eval stack.
    let private exitCodeOf (outcome : RunOutcome) : int =
        let terminalState, terminatingThread =
            match outcome with
            | RunOutcome.NormalExit (state, thread) -> state, thread
            | RunOutcome.ProcessExit (state, thread) -> state, thread
            | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
        | [] -> failwith "expected the guest to return a value, but it returned void"
        | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> i
        | ret :: _ -> failwith $"expected the guest to return an int, but it returned %O{ret}"

    let private runOn (name : string) (platform : SimulatedUnixPlatform) : int =
        let image = Roslyn.compile [ guest ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

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
        |> exitCodeOf

    /// Both columns, from the interpreter rather than from the model directly:
    /// this is what makes the handler's read of `Kernel.UnixPlatform` load-bearing.
    /// A handler hardcoding either constant fails one of the two cases, and one
    /// answering from anything but the flavour fails at least one.
    [<Test>]
    let ``the answer follows the simulated flavour`` () : unit =
        runOn "DualModeIPv4PacketInfoLinux.cs" SimulatedUnixPlatform.linuxX64
        |> shouldEqual 1

        runOn "DualModeIPv4PacketInfoDarwin.cs" SimulatedUnixPlatform.macOsArm64
        |> shouldEqual 0

    /// The per-flavour constants against a real shim, so neither is a literal
    /// measured once and then trusted forever: a macOS dev box checks the Darwin
    /// column and a Linux CI runner checks the Linux column, on every run.
    ///
    /// Calling the host's own `libSystem.Native` is fine in a *test*; the
    /// prohibition on reading the host applies to the product.
    [<Test>]
    let ``the model agrees with this host's own shim`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let measured = hostPlatformSupportsDualModeIPv4PacketInfo ()

            // The upstream body returns a literal 1 or 0, so anything else means
            // the symbol resolved to something other than the function being
            // modelled.
            if measured <> 0 && measured <> 1 then
                failwith
                    $"SystemNative_PlatformSupportsDualModeIPv4PacketInfo returned %d{measured} on this host, but upstream (pal_networking.c) can only return 0 or 1."

            (measured = 1)
            |> shouldEqual (SimulatedUnixPlatform.supportsDualModeIPv4PacketInfo (HostPlatform.platformOf flavour))
        )
