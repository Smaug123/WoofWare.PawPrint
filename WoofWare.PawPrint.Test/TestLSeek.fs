namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// The parts of `SystemNative_LSeek`'s contract that `sourcesPure/ReadSeekSeeded.cs` and
/// `sourcesImpure/LSeekRawSeeded.cs` cannot reach, for three different reasons.
///
///  * **The refusals.** `SEEK_DATA`/`SEEK_HOLE` and `SEEK_END` on a directory abort the interpreter
///    rather than returning to the guest, so no exit code can assert them.
///  * **The Darwin arms.** PawPrint defaults to a Linux-flavoured kernel, so the guests above only
///    ever exercise Linux's answers. The overflow errno in particular is *indistinguishable* from
///    the negative-result errno under Linux — both EINVAL — so a model that failed to tell the two
///    faults apart would pass every guest.
///  * **Offsets no host filesystem agrees about.** `lseek(f, INT64_MAX, SEEK_SET)` succeeds under
///    PawPrint, matching tmpfs and APFS but not ext4, whose `s_maxbytes` stops at `0xffffffff000`.
///    A differential guest asserting it would pass on a macOS dev box and fail on a Linux CI runner
///    for a reason having nothing to do with the kernel, so it is pinned against the model here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLSeek =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// One five-byte file and one directory.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                let name (s : string) = FileName.parseOrFail "test seed" s

                Map.ofList
                    [
                        name "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                        name "d", SeedEntry.directory Map.empty
                    ]
        }

    /// The same seed under a Darwin-flavoured kernel.
    let private darwin : KernelConfig =
        { seed with
            UnixPlatform = SimulatedUnixPlatform.macOsArm64
        }

    /// Raw `SystemNative_Open` and `SystemNative_LSeek`, so a test can pass whence values the BCL
    /// never sends. `%s` is spliced with the body of `Main`.
    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_LSeek", SetLastError = true)]
    static extern long LSeek(IntPtr fd, long offset, int whence);

    const int SEEK_SET = 0;
    const int SEEK_CUR = 1;
    const int SEEK_END = 2;

    static unsafe IntPtr OpenPath(string name)
    {{
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }}

    static bool Rejected(IntPtr fd, long offset, int whence, int expectedErrno)
    {{
        Marshal.SetLastSystemError(0);
        long r = LSeek(fd, offset, whence);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }}

    static unsafe int Main(string[] args)
    {{
{body}
    }}
}}
"""

    /// The exit code a terminated guest left on its terminating thread's eval stack.
    let exitCodeOf (outcome : RunOutcome) : int =
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

    /// `SEEK_DATA` and `SEEK_HOLE` ask where a file's holes are, which the emulated filesystem does
    /// not represent — and the two platforms transpose their numbers, so the raw value does not even
    /// name one operation. Refused rather than answered.
    [<Test>]
    let ``SEEK_DATA and SEEK_HOLE are refused`` () : unit =
        for whence in [ 3 ; 4 ] do
            let source =
                guest
                    $"""
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        LSeek(f, 0, {whence});
        return 0;
"""

            let exn =
                Assert.Catch (fun () -> run $"LSeekWhence{whence}.cs" source |> ignore<RunOutcome>)

            exn.Message |> shouldContainText "SystemNative_LSeek"
            exn.Message |> shouldContainText "sparseness"
            // The message names the operation *the simulated platform* means by this number:
            // under Linux, 3 is SEEK_DATA.
            exn.Message
            |> shouldContainText (if whence = 3 then "SEEK_DATA" else "SEEK_HOLE")

    /// ...and Darwin means the other one by the same number, so the message follows the flavour
    /// rather than hard-coding Linux's reading. Without this, a message that simply said "SEEK_DATA"
    /// for whence 3 would pass the test above and mislead on the platform where it is SEEK_HOLE.
    [<Test>]
    let ``the refused whence is named per the simulated platform`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        LSeek(f, 0, 3);
        return 0;
"""

        let exn =
            Assert.Catch (fun () -> runOn darwin "LSeekWhence3Darwin.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "SEEK_HOLE"

    /// The refusal sits *after* the descriptor and seekability checks, not before them. Measured,
    /// `lseek(badfd, 0, 3)` is EBADF and `lseek(pipe, 0, 3)` is ESPIPE on both platforms — so a
    /// handler refusing whence 3 up front would crash on two inputs every real kernel answers.
    [<Test>]
    let ``a bad descriptor is answered before the whence is interpreted`` () : unit =
        let source =
            guest
                """
        // EBADF, not a refusal: the fd is resolved first.
        if (!Rejected(new IntPtr(4242), 0, 3, 9)) return 1;
        if (!Rejected(new IntPtr(4242), 0, 4, 9)) return 2;
        // ESPIPE, not a refusal: seekability is decided before the whence means anything.
        if (!Rejected(new IntPtr(0), 0, 3, 29)) return 3;
        if (!Rejected(new IntPtr(1), 0, 4, 29)) return 4;
        return 0;
"""

        run "LSeekWhenceOrder.cs" source |> exitCodeOf |> shouldEqual 0

    /// A directory has no size PawPrint will state: measured, `lseek(dir, 0, SEEK_END)` is EINVAL on
    /// Linux/tmpfs, 4096 on Linux/ext4 and 64 on macOS/APFS. `FStat` reports 4096 because `stat`
    /// must fill the field in; nothing forces this one, so it is refused.
    [<Test>]
    let ``SEEK_END on a directory is refused`` () : unit =
        let source =
            guest
                """
        IntPtr d = OpenPath("d");
        if (d == new IntPtr(-1)) return 1;
        LSeek(d, 0, SEEK_END);
        return 0;
"""

        let exn = Assert.Catch (fun () -> run "LSeekDirEnd.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "SystemNative_LSeek"
        exn.Message |> shouldContainText "directory"
        exn.Message |> shouldContainText "SEEK_END"

    /// ...and only `SEEK_END`. `SEEK_SET` and `SEEK_CUR` on a directory are portable — identical on
    /// tmpfs, ext4 and APFS — so refusing them would be an over-refusal, and one that "refuse to
    /// seek a directory" would commit silently.
    [<Test>]
    let ``SEEK_SET and SEEK_CUR on a directory are answered`` () : unit =
        let source =
            guest
                """
        IntPtr d = OpenPath("d");
        if (d == new IntPtr(-1)) return 1;
        if (LSeek(d, 0, SEEK_CUR) != 0) return 2;
        if (LSeek(d, 7, SEEK_SET) != 7) return 3;
        if (LSeek(d, 0, SEEK_CUR) != 7) return 4;
        if (LSeek(d, -1, SEEK_CUR) != 6) return 5;
        // A negative result is still EINVAL, so the arithmetic is running rather than being skipped.
        if (!Rejected(d, -7, SEEK_CUR, 22)) return 6;
        return 0;
"""

        run "LSeekDirSetCur.cs" source |> exitCodeOf |> shouldEqual 0

    /// Darwin decides seekability *before* it validates the whence, and Linux the other way round.
    /// `sourcesImpure/LSeekRawSeeded.cs` pins the Linux half — an unseekable descriptor with a
    /// nonsense whence is EINVAL there — and this is the other, which no guest run under the default
    /// flavour can reach. Without both, either ordering could be flipped to match the other and the
    /// suite would stay green.
    [<Test>]
    let ``Darwin decides seekability before the whence is validated`` () : unit =
        let source =
            guest
                """
        // ESPIPE, where Linux answers EINVAL.
        if (!Rejected(new IntPtr(0), 0, 99, 29)) return 1;
        if (!Rejected(new IntPtr(1), 0, -1, 29)) return 2;
        // ...and still ESPIPE when the offset would also have overflowed, so this is the whence
        // losing rather than the least-severe fault winning.
        if (!Rejected(new IntPtr(0), long.MaxValue, 99, 29)) return 3;
        // The descriptor itself still precedes both: a bad fd with a bad whence is EBADF.
        if (!Rejected(new IntPtr(4242), 0, 99, 9)) return 4;
        // ...and a *valid* whence on an unseekable descriptor is ESPIPE too, so the rows above are
        // not passing merely because ESPIPE is this handler's favourite answer.
        if (!Rejected(new IntPtr(0), 0, SEEK_CUR, 29)) return 5;
        return 0;
"""

        runOn darwin "LSeekDarwinOrder.cs" source |> exitCodeOf |> shouldEqual 0

    /// Under Darwin, an offset that leaves `int64` is EOVERFLOW rather than EINVAL. This is the only
    /// place that distinction is observable: under Linux both faults report EINVAL, so a model that
    /// conflated "overflowed" with "landed below zero" — which is exactly what unchecked `int64`
    /// addition does, since the wrap lands negative — would pass every Linux-flavoured guest.
    [<Test>]
    let ``Darwin reports EOVERFLOW where Linux reports EINVAL`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        // `f` is five bytes, so this lands one past INT64_MAX.
        if (!Rejected(f, long.MaxValue - 4, SEEK_END, 84)) return 2;
        // A *negative* result is EINVAL on Darwin too, so the two faults are genuinely being told
        // apart rather than the errno being switched wholesale by platform.
        if (!Rejected(f, -1, SEEK_SET, 22)) return 3;
        // ...and the boundary itself is fine: INT64_MAX exactly is a legal position.
        if (LSeek(f, long.MaxValue - 5, SEEK_END) != long.MaxValue) return 4;
        return 0;
"""

        runOn darwin "LSeekOverflowDarwin.cs" source |> exitCodeOf |> shouldEqual 0

    /// PawPrint's filesystem is in memory, so it has no `s_maxbytes` ceiling: every non-negative
    /// `int64` is a position. Measured, that matches tmpfs and APFS; ext4 stops at `0xffffffff000`,
    /// which is why this cannot be a differential guest — the oracle's answer depends on which
    /// filesystem the test host happens to be running on.
    [<Test>]
    let ``an offset far beyond any real file is accepted`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        if (LSeek(f, long.MaxValue, SEEK_SET) != long.MaxValue) return 2;
        if (LSeek(f, 0, SEEK_CUR) != long.MaxValue) return 3;
        // ext4's ceiling, comfortably inside what tmpfs and APFS accept.
        if (LSeek(f, 0xffffffff000L, SEEK_SET) != 0xffffffff000L) return 4;
        // Reading there transfers nothing, rather than erroring or rewinding.
        if (LSeek(f, long.MaxValue, SEEK_SET) != long.MaxValue) return 5;
        // Adding to INT64_MAX overflows, and is rejected rather than wrapped.
        if (!Rejected(f, 1, SEEK_CUR, 22)) return 6;
        if (LSeek(f, 0, SEEK_CUR) != long.MaxValue) return 7;
        return 0;
"""

        run "LSeekHugeOffset.cs" source |> exitCodeOf |> shouldEqual 0
