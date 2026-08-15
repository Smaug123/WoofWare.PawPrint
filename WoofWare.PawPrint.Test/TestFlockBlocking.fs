namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `SystemNative_FLock` refuses a *blocking* request that cannot be granted, because waiting for
/// the holder needs the scheduler to park and wake a thread. That refusal has no guest that can
/// assert it — a `failwith` aborts the interpreter rather than reaching the guest's exit code — so
/// it would otherwise be a claim with nothing behind it. These tests drive it directly.
///
/// The distinction being pinned is *narrow* and easy to get wrong in the permissive direction: a
/// blocking request that can be satisfied must still succeed, since that is what a hand-rolled
/// P/Invoke most naturally writes. Only genuine contention is refused. CoreLib never reaches
/// either, because `SafeFileHandle.Init` always sets `LOCK_NB`.
///
/// Note the guests below are single-threaded and hold the conflicting lock themselves, so on a real
/// Linux kernel they would *hang forever* rather than return anything. That is not a defect in the
/// tests: it is why the refusal is worth having, and it is also why these cannot be differential
/// cases — the oracle would never terminate.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFlockBlocking =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// One regular file, which is all these guests open.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                Map.ofList
                    [
                        FileName.parseOrFail "test seed" "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                    ]
        }

    /// The shared preamble: raw `SystemNative_Open` and `SystemNative_FLock`, so the test can ask
    /// for operation bits the BCL never sends. `%s` is spliced with the body of `Main`.
    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_FLock", SetLastError = true)]
    static extern int FLock(IntPtr fd, int operation);

    const int LOCK_SH = 1;
    const int LOCK_EX = 2;
    const int LOCK_NB = 4;
    const int LOCK_UN = 8;

    static unsafe IntPtr OpenF()
    {{
        byte* path = stackalloc byte[2];
        path[0] = (byte)'f';
        path[1] = 0;
        return Open(path, 0, 0);
    }}

    static unsafe int Main(string[] args)
    {{
{body}
    }}
}}
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

    let private run (name : string) (source : string) : RunOutcome =
        let image = Roslyn.compile [ source ]

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
                        Kernel = seed
                    }
            }

    /// The refusal itself: two descriptions on one file, the first holding an exclusive lock, and
    /// the second asking for one *without* `LOCK_NB`.
    [<Test>]
    let ``a blocking request that conflicts is refused loudly`` () : unit =
        let source =
            guest
                """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;
        // No LOCK_NB: a real kernel would park this thread until `a` released.
        return FLock(b, LOCK_EX);
"""

        let exn = Assert.Catch (fun () -> run "FlockBlocks.cs" source |> ignore<RunOutcome>)

        // Named precisely enough that a failing run says which fd wanted what, rather than merely
        // that flock was unhappy.
        exn.Message |> shouldContainText "SystemNative_FLock"
        exn.Message |> shouldContainText "blocking exclusive lock"
        exn.Message |> shouldContainText "issue #956"

    /// The other half, and the reason the refusal is conditional rather than "no `LOCK_NB` is
    /// unsupported": an uncontended blocking request is an ordinary success. Without this, the
    /// simplest wrong implementation — refuse whenever `LOCK_NB` is absent — passes the test above.
    [<Test>]
    let ``a blocking request that can be granted succeeds`` () : unit =
        let source =
            guest
                """
        IntPtr a = OpenF();
        // Nothing else holds a lock, so this is grantable and must not be refused.
        if (FLock(a, LOCK_EX) != 0) return 1;
        // Downgrading to shared is also grantable: a description's own lock is not an obstacle.
        if (FLock(a, LOCK_SH) != 0) return 2;
        if (FLock(a, LOCK_UN) != 0) return 3;
        return 0;
"""

        run "FlockBlockingGranted.cs" source |> exitCodeOf |> shouldEqual 0

    /// A blocking *shared* request refused for the same reason, so the message is not
    /// exclusive-only, and a compatible shared/shared pair is still granted.
    [<Test>]
    let ``a blocking shared request conflicts only with an exclusive holder`` () : unit =
        let conflicting =
            guest
                """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;
        return FLock(b, LOCK_SH);
"""

        let exn =
            Assert.Catch (fun () -> run "FlockBlocksShared.cs" conflicting |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "blocking shared lock"

        let compatible =
            guest
                """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_SH | LOCK_NB) != 0) return 1;
        // Shared against shared: grantable, so blocking is irrelevant.
        if (FLock(b, LOCK_SH) != 0) return 2;
        return 0;
"""

        run "FlockSharedGranted.cs" compatible |> exitCodeOf |> shouldEqual 0
