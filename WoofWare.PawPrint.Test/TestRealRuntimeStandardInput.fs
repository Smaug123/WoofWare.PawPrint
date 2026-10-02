namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open NUnit.Framework
open FsUnitTyped
open WoofWare.PosixKernel

/// The oracle writes a guest's standard input on a thread of its own, because
/// the write blocks while the pipe is full. These are what that buys: a guest
/// that reads every byte of more than a pipe holds sees them all, and one that
/// never reads exits rather than leaving the oracle blocked on the write.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestRealRuntimeStandardInput =

    let private payload : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init (1024 * 1024) (fun i -> byte (i % 251)))

    [<Test>]
    let ``a guest reads every byte of a standard input larger than a pipe`` () : unit =
        let image =
            Roslyn.compile
                [
                    """
using System;
using System.IO;

class Program
{
    static int Main()
    {
        using Stream stdin = Console.OpenStandardInput();
        byte[] buffer = new byte[10000];
        long position = 0;
        int got;
        while ((got = stdin.Read(buffer, 0, buffer.Length)) > 0)
        {
            for (int i = 0; i < got; i++)
            {
                if (buffer[i] != (byte)((position + i) % 251)) return 2;
            }
            position += got;
        }
        return position == 1024 * 1024 ? 0 : 1;
    }
}
"""
                ]

        RealRuntime.executeWithSeed FileSystemSeed.empty [] payload [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 0)

    [<Test>]
    let ``a guest that never reads its standard input still finishes`` () : unit =
        let image =
            Roslyn.compile
                [
                    """
class Program
{
    static int Main() => 7;
}
"""
                ]

        RealRuntime.executeWithTimeoutAndSeed (TimeSpan.FromSeconds 60.0) FileSystemSeed.empty [] payload [||] image
        |> shouldEqual (RealRuntimeResult.NormalExit 7)

    /// The case the separate thread exists for: a guest that neither reads nor
    /// exits. Written on the calling thread, the bytes would block it on the
    /// full pipe before it ever reached the timeout.
    [<Test>]
    let ``a guest that never reads and never exits is timed out`` () : unit =
        let image =
            Roslyn.compile
                [
                    """
using System.Threading;

class Program
{
    static int Main()
    {
        Thread.Sleep(Timeout.Infinite);
        return 0;
    }
}
"""
                ]

        let run =
            System.Threading.Tasks.Task.Run (fun () ->
                try
                    RealRuntime.executeWithTimeoutAndSeed
                        (TimeSpan.FromSeconds 3.0)
                        FileSystemSeed.empty
                        []
                        payload
                        [||]
                        image
                    |> fun result -> Ok result
                with e ->
                    Error e.Message
            )

        run.Wait (TimeSpan.FromSeconds 60.0) |> shouldEqual true

        match run.Result with
        | Error message -> message |> shouldContainText "did not terminate within 3s"
        | Ok result -> failwith $"expected the guest to be timed out, but it finished: %A{result}"
