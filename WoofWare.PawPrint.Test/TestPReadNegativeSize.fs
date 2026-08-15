namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `SystemNative_PRead` refuses a negative `bufferSize` rather than answering, because what a real
/// kernel does with one is not a fact PawPrint can state: the C shim casts it to an unsigned ~4 GB
/// count, and measured, macOS answers EINVAL while Linux answers EFAULT — Linux's answer depending
/// on how far the guest's buffer happens to be mapped, which PawPrint does not model to that
/// fidelity.
///
/// A `failwith` aborts the interpreter rather than reaching the guest's exit code, so no guest can
/// assert it and it would otherwise be a claim with nothing behind it. These tests drive it.
///
/// Note this is `PRead` specifically. `SystemNative_Read` goes through `Common_Read` in
/// `pal_io_common.h`, which *does* have a negative-size guard and answers ERANGE; `PRead` does not
/// go through it, so the two entry points genuinely differ and the refusal is not an oversight in
/// one of them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPReadNegativeSize =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                Map.ofList
                    [
                        FileName.parseOrFail "test seed" "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                    ]
        }

    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long fileOffset);

    static unsafe IntPtr OpenF()
    {{
        byte* path = stackalloc byte[2];
        path[0] = (byte)'f';
        path[1] = 0;
        return Open(path, 0, 0);
    }}

    static unsafe int Main(string[] args)
    {{
        byte* buf = stackalloc byte[64];
{body}
    }}
}}
"""

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

    /// Every other argument valid — a live descriptor on a real file, a dereferenceable buffer, a
    /// non-negative offset — so the refusal is provably the negative size and not an earlier guard
    /// firing first.
    [<Test>]
    let ``a negative bufferSize is refused loudly`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenF();
        return PRead(f, buf, -1, 0);
"""

        let exn =
            Assert.Catch (fun () -> run "PReadNegativeSize.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "SystemNative_PRead"
        exn.Message |> shouldContainText "bufferSize -1"
        // The message must carry the measurement, since it is the storage medium for it.
        exn.Message |> shouldContainText "macOS answers EINVAL and Linux answers EFAULT"

    /// The control: the same call with a *non-negative* size is served, so the refusal is narrow
    /// rather than "PRead refuses sizes". Without this, a handler that refused every call would
    /// pass the test above.
    [<Test>]
    let ``a non-negative bufferSize is served`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenF();
        if (PRead(f, buf, 5, 0) != 5) return 1;
        if (buf[0] != 'h') return 2;
        // Zero is not negative, and is the boundary the refusal must not swallow.
        if (PRead(f, buf, 0, 0) != 0) return 3;
        return 0;
"""

        run "PReadZeroSize.cs" source |> exitCodeOf |> shouldEqual 0

    /// The refusal precedes every other check, so it fires even on a call that a real kernel would
    /// have rejected for a different reason first. That is a deliberate over-refusal on a
    /// two-fault input — a real kernel answers EBADF here, since fd lookup precedes buffer use —
    /// and it is asserted rather than merely commented, because a green suite cannot show it.
    [<Test>]
    let ``a negative bufferSize beats a bad descriptor`` () : unit =
        let source =
            guest
                """
        return PRead(new IntPtr(4242), buf, -1, 0);
"""

        let exn =
            Assert.Catch (fun () -> run "PReadNegativeSizeBadFd.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "bufferSize -1"

    /// ...and beats a negative *offset*, which PawPrint would otherwise answer with EINVAL. Two
    /// refusable things at once, and the size is the one that wins.
    [<Test>]
    let ``a negative bufferSize beats a negative offset`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenF();
        return PRead(f, buf, -1, -1);
"""

        let exn =
            Assert.Catch (fun () -> run "PReadNegativeSizeAndOffset.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "bufferSize -1"

    /// A negative offset on its own is an ordinary EINVAL rather than a crash, so the refusal
    /// above is attributable to the size.
    [<Test>]
    let ``a negative offset alone is EINVAL`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenF();
        Marshal.SetLastSystemError(0);
        if (PRead(f, buf, 5, -1) != -1) return 1;
        if (Marshal.GetLastSystemError() != 22) return 2;
        return 0;
"""

        run "PReadNegativeOffset.cs" source |> exitCodeOf |> shouldEqual 0

    /// The *order* `pread`'s checks run in differs between the two platforms, and PawPrint follows
    /// whichever the configured kernel claims to be. Both orders are measured in full
    /// (scratchpad/preadpairs.c), which is why this models Darwin rather than refusing it as
    /// `SystemNative_FLock` does — there, the Darwin return codes are known but the lock state they
    /// leave behind is not.
    ///
    /// Only two-fault inputs can tell the orders apart, so these are the tests that keep the Darwin
    /// branch from being dead code, and the Linux assertions in `PReadRawSeeded.cs` are the other
    /// half of the same claim.
    module CheckOrder =

        let private darwin : KernelConfig =
            { seed with
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
            }

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

        /// The three rows that distinguish the orders. Darwin resolves the descriptor and its
        /// seekability first, so a negative offset on a bad or unseekable descriptor reports the
        /// descriptor's problem; a negative offset on a *directory* still reports EINVAL, because
        /// EISDIR follows the offset check on both platforms.
        [<Test>]
        let ``Darwin resolves the descriptor before validating the offset`` () : unit =
            let source =
                guest
                    """
        IntPtr f = OpenF();
        // Darwin: the descriptor is looked up first, so a bad one wins.
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(4242), buf, 5, -1) != -1) return 1;
        if (Marshal.GetLastSystemError() != 9) return 2;      // EBADF
        // ...and an unseekable one likewise. fd 0 rather than fd 1: stdin is the *read* end
        // of its pipe, so it fails only the seekability test. fd 1 is a write end, which
        // Darwin rejects as unreadable before it gets that far -- see the sibling test.
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(0), buf, 5, -1) != -1) return 3;
        // ESPIPE is 29 on *both* platforms -- it sits in the portable 1-34 band, unlike
        // EAGAIN, which is the one errno the two Unixes transpose (11 against 35). So the
        // divergence being pinned here is purely the check *order*, not the numbering.
        if (Marshal.GetLastSystemError() != 29) return 4;
        // The write end under Darwin: unreadable beats unseekable, so EBADF -- and it still
        // beats the negative offset, which is the ordering this test is about.
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(1), buf, 5, -1) != -1) return 9;
        if (Marshal.GetLastSystemError() != 9) return 10;
        // A negative offset on a good descriptor is still EINVAL...
        Marshal.SetLastSystemError(0);
        if (PRead(f, buf, 5, -1) != -1) return 5;
        if (Marshal.GetLastSystemError() != 22) return 6;     // EINVAL
        // ...and the ordinary read still works.
        if (PRead(f, buf, 5, 0) != 5) return 7;
        if (buf[0] != 'h' || buf[4] != 'o') return 8;
        return 0;
"""

            runOn darwin "PReadDarwinOrder.cs" source |> exitCodeOf |> shouldEqual 0

        /// The same three calls under Linux, which answers EINVAL for all of them because it
        /// validates the offset before looking the descriptor up at all. Without this the Darwin
        /// test above would pass for an implementation that had simply got Linux wrong.
        [<Test>]
        let ``Linux validates the offset before resolving the descriptor`` () : unit =
            let source =
                guest
                    """
        IntPtr f = OpenF();
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(4242), buf, 5, -1) != -1) return 1;
        if (Marshal.GetLastSystemError() != 22) return 2;     // EINVAL, not EBADF
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(1), buf, 5, -1) != -1) return 3;
        if (Marshal.GetLastSystemError() != 22) return 4;     // EINVAL, not ESPIPE
        Marshal.SetLastSystemError(0);
        if (PRead(f, buf, 5, -1) != -1) return 5;
        if (Marshal.GetLastSystemError() != 22) return 6;
        return 0;
"""

            run "PReadLinuxOrder.cs" source |> exitCodeOf |> shouldEqual 0

        /// Single-fault reads are identical on both platforms, so a Darwin-configured guest reads
        /// files exactly as a Linux one does. This is what makes the ordering difference the *only*
        /// divergence, rather than one symptom of a wider one.
        [<Test>]
        let ``ordinary reads are identical under either flavour`` () : unit =
            let source =
                guest
                    """
        IntPtr f = OpenF();
        if (PRead(f, buf, 64, 0) != 5) return 1;
        if (buf[0] != 'h' || buf[4] != 'o') return 2;
        if (PRead(f, buf, 64, 3) != 2) return 3;
        if (buf[0] != 'l' || buf[1] != 'o') return 4;
        if (PRead(f, buf, 64, 5) != 0) return 5;
        if (PRead(f, buf, 0, 0) != 0) return 6;
        return 0;
"""

            runOn darwin "PReadDarwinOrdinary.cs" source |> exitCodeOf |> shouldEqual 0
            run "PReadLinuxOrdinary.cs" source |> exitCodeOf |> shouldEqual 0

        /// A standard stream fails two tests at once for stdout and stderr — neither seekable nor
        /// open for reading — and the platforms break that tie differently. Measured
        /// (scratchpad/preaddir.c): the write end of a pipe is ESPIPE on Linux and EBADF on
        /// Darwin, while a *seekable* write-only descriptor is EBADF on both, so the divergence is
        /// about the tie rather than about readability generally.
        [<Test>]
        let ``an output stream is ESPIPE under Linux and EBADF under Darwin`` () : unit =
            let source =
                guest
                    """
        // fd 0 is the read end: unseekable but readable, so ESPIPE on both.
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(0), buf, 5, 0) != -1) return 1;
        if (Marshal.GetLastSystemError() != 29) return 2;
        // fds 1 and 2 are write ends: unseekable *and* unreadable.
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(1), buf, 5, 0) != -1) return 3;
        int outErrno = Marshal.GetLastSystemError();
        Marshal.SetLastSystemError(0);
        if (PRead(new IntPtr(2), buf, 5, 0) != -1) return 4;
        int errErrno = Marshal.GetLastSystemError();
        // stdout and stderr must agree with each other whatever the platform says.
        if (outErrno != errErrno) return 5;
        // The exit code carries the errno so the caller can assert the platform's answer.
        return outErrno;
"""

            // Linux lets unseekability win, so all three streams answer ESPIPE.
            run "PReadStreamLinux.cs" source |> exitCodeOf |> shouldEqual 29
            // Darwin lets unreadability win for the two write ends.
            runOn darwin "PReadStreamDarwin.cs" source |> exitCodeOf |> shouldEqual 9
