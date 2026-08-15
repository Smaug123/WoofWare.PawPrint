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

    let private runOn (kernel : KernelConfig) (name : string) (source : string) : RunOutcome =
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
                        Kernel = kernel
                    }
            }

    let private run (name : string) (source : string) : RunOutcome = runOn seed name source

    /// The same seed under a Darwin-flavoured kernel, which is what the refusals below need.
    let private darwin : KernelConfig =
        { seed with
            UnixPlatform = SimulatedUnixPlatform.macOsArm64
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

    /// PawPrint models Linux's `flock` and refuses under a Darwin-flavoured kernel wherever the
    /// two genuinely diverge, rather than applying Linux's rule to a kernel that told the guest it
    /// is Darwin. The alternative — modelling Darwin — needs measurements nobody has taken: what
    /// is known is Darwin's *return codes*, not the lock state they leave behind, and `FlockMode`
    /// would have to commit to the latter.
    ///
    /// These tests exist because a refusal is invisible to a green suite: nothing else in the
    /// repository configures a Darwin platform and then calls `flock`, so without them the arms
    /// could be dead, or could fire on operations the two platforms actually agree about.
    module Darwin =

        /// The operations both platforms handle identically must still work under Darwin. This is
        /// the control, and it is what stops the refusal being "Darwin cannot use flock at all":
        /// every operation CoreLib actually sends is in here.
        [<Test>]
        let ``the operations both platforms agree on are served under Darwin`` () : unit =
            let source =
                guest
                    """
        IntPtr a = OpenF();
        IntPtr b = OpenF();
        if (FLock(a, LOCK_SH | LOCK_NB) != 0) return 1;
        // Shared against shared, which both platforms grant.
        if (FLock(b, LOCK_SH | LOCK_NB) != 0) return 2;
        if (FLock(a, LOCK_UN) != 0) return 3;
        if (FLock(b, LOCK_UN) != 0) return 4;
        // Exclusive, then contention, which both report as EWOULDBLOCK -- and under Darwin's
        // errno numbering, which is 35 rather than Linux's 11.
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 5;
        Marshal.SetLastSystemError(0);
        if (FLock(b, LOCK_EX | LOCK_NB) != -1) return 6;
        if (Marshal.GetLastSystemError() != 35) return 7;
        if (FLock(a, LOCK_UN) != 0) return 8;
        return 0;
"""

            runOn darwin "FlockDarwinAgreed.cs" source |> exitCodeOf |> shouldEqual 0

        /// Operation validation. Linux rejects a malformed operation with EINVAL; Darwin answers
        /// EBADF for some and *succeeds* for others, and what lock a Darwin success leaves behind
        /// is unmeasured.
        [<Test>]
        let ``a malformed operation is refused under Darwin`` () : unit =
            let source =
                guest
                    """
        IntPtr a = OpenF();
        return FLock(a, LOCK_SH | LOCK_EX);
"""

            let exn =
                Assert.Catch (fun () -> runOn darwin "FlockDarwinMalformed.cs" source |> ignore<RunOutcome>)

            exn.Message |> shouldContainText "SystemNative_FLock"
            exn.Message |> shouldContainText "malformed"
            exn.Message |> shouldContainText "issue #956"

        /// ...and the same operation under Linux is an ordinary EINVAL rather than a crash, so the
        /// refusal is attributable to the flavour and not to the operation.
        [<Test>]
        let ``the same malformed operation is EINVAL under Linux`` () : unit =
            let source =
                guest
                    """
        IntPtr a = OpenF();
        Marshal.SetLastSystemError(0);
        if (FLock(a, LOCK_SH | LOCK_EX) != -1) return 1;
        if (Marshal.GetLastSystemError() != 22) return 2;
        return 0;
"""

            run "FlockLinuxMalformed.cs" source |> exitCodeOf |> shouldEqual 0

        /// `flock` on a pipe: 0 on Linux, ENOTSUP on Darwin. PawPrint models fds 0/1/2 as pipes.
        [<Test>]
        let ``flock on a standard stream is refused under Darwin`` () : unit =
            let source =
                guest
                    """
        return FLock(new IntPtr(1), LOCK_EX | LOCK_NB);
"""

            let exn =
                Assert.Catch (fun () -> runOn darwin "FlockDarwinStream.cs" source |> ignore<RunOutcome>)

            exn.Message |> shouldContainText "standard stream"
            exn.Message |> shouldContainText "ENOTSUP"

        /// Converting a lock: should the conversion fail, Linux has already dropped the old lock
        /// and Darwin has not. Refused on the *request*, so it fires whether or not this
        /// particular conversion would have been granted -- there is no way to know that before
        /// deciding which platform's rule to apply.
        [<Test>]
        let ``converting a held lock is refused under Darwin`` () : unit =
            let source =
                guest
                    """
        IntPtr a = OpenF();
        if (FLock(a, LOCK_SH | LOCK_NB) != 0) return 1;
        return FLock(a, LOCK_EX | LOCK_NB);
"""

            let exn =
                Assert.Catch (fun () -> runOn darwin "FlockDarwinConvert.cs" source |> ignore<RunOutcome>)

            exn.Message |> shouldContainText "converting a lock it already holds"

        /// The narrowness of that arm: an acquire by a description holding *nothing* is not a
        /// conversion, so it is served rather than refused. Without this the refusal could be
        /// "Darwin cannot acquire locks", which would also pass the test above.
        [<Test>]
        let ``a first acquisition is not a conversion and is served under Darwin`` () : unit =
            let source =
                guest
                    """
        IntPtr a = OpenF();
        if (FLock(a, LOCK_EX | LOCK_NB) != 0) return 1;
        // Releasing is not a conversion either, so it is served.
        if (FLock(a, LOCK_UN) != 0) return 2;
        // ...and having released, acquiring again is once more a first acquisition.
        if (FLock(a, LOCK_SH | LOCK_NB) != 0) return 3;
        return 0;
"""

            runOn darwin "FlockDarwinFirstAcquire.cs" source |> exitCodeOf |> shouldEqual 0
