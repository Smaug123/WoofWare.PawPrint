namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// A guest that keeps opening descriptors reaches the kernel library's bound
/// (`SimulatedUnixPlatform.descriptorBound`), past which what a real process
/// answers depends on an `RLIMIT_NOFILE` the library does not model. The
/// handler that made the call fails the run, naming the bound, rather than
/// answering for some limit.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDescriptorBoundRefusal =

    let private source (call : string) : string =
        $$"""
using System;
using System.Runtime.InteropServices;

public class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Dup")]
    static extern IntPtr Dup(IntPtr oldFd);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open")]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    public static unsafe int Main(string[] args)
    {
        byte* path = stackalloc byte[2];
        path[0] = (byte)'.';
        path[1] = 0;

        for (int i = 0; i < 2000; i++)
        {
            long fd = (long){{call}};
            if (fd < 0) return 1;
        }

        return 2;
    }
}
"""

    let private runRefused (name : string) (call : string) : GuestFailureException =
        let image = Roslyn.compile [ source call ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let runtimes = FrameworkUnderTest.runtimeDirs ()

        let config =
            { HostConfig.Default runtimes with
                Guest =
                    { GuestConfig.Default runtimes with
                        Kernel =
                            { KernelConfig.Default with
                                UnixPlatform = SimulatedUnixPlatform.macOsArm64
                            }
                    }
            }

        Assert.Throws<GuestFailureException> (fun () ->
            BoundedRun.runWith loggerFactory BoundedRun.defaultMaxSteps name (Some name) peImage config
            |> ExpectRun.ended
            |> ignore<RunOutcome>
        )

    [<Test>]
    let ``SystemNative_Dup past the bound fails the run, naming it`` () : unit =
        let exc = runRefused "DupPastBound.cs" "Dup((IntPtr)0)"
        exc.Message |> shouldContainText "SystemNative_Dup"
        exc.Message |> shouldContainText "at or above 256"

    [<Test>]
    let ``SystemNative_Open past the bound fails the run, naming it`` () : unit =
        let exc = runRefused "OpenPastBound.cs" "Open(path, 0, 0)"
        exc.Message |> shouldContainText "SystemNative_Open"
        exc.Message |> shouldContainText "at or above 256"
