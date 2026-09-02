namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Where each read/write entry point screens its buffer against the address
/// space, which is the one part of their contract the two simulated platforms
/// disagree about.
///
/// `sourcesPure` cannot hold these rows: the pure suite's oracle is the host
/// kernel, so a high-address row answers 0 on a macOS dev box and EFAULT on a
/// Linux CI runner. A guest here names its flavour instead.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUserBufferFault =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// One five-byte file and one directory.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                let name (s : string) =
                    DirectoryEntryName.parseOrFail "test seed" s

                Map.ofList
                    [
                        name "f",
                        SeedEntry.file (System.Text.Encoding.UTF8.GetBytes "hello" |> ImmutableArray.CreateRange)
                        name "d", SeedEntry.directory Map.empty
                    ]
        }

    let private darwin : KernelConfig =
        { seed with
            UnixPlatform = SimulatedUnixPlatform.macOsArm64
        }

    /// Raw P/Invokes, because every row here is one the BCL cannot produce: its
    /// own wrappers pass real spans, so no managed call site can hand a
    /// syscall an address outside the user address space.
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

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Read", SetLastError = true)]
    static extern unsafe int Read(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long offset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    const int SEEK_END = 2;

    const int EBADF = 9;
    const int EFAULT = 14;
    const int EISDIR = 21;
    const int EINVAL = 22;
    const int ESPIPE = 29;
    const int ERANGE = 34;

    // The top of the address space: the sum of address and length wraps, which
    // no kernel accepts, and even at zero length it is far past TASK_SIZE_MAX.
    static unsafe byte* Wild => (byte*)(-1);

    // Well below TASK_SIZE_MAX, so accepted, but far above anything this guest
    // has mapped: the screen passes it and the operation then transfers
    // nothing.
    static unsafe byte* HighButUser => (byte*)0x0000_7FFF_0000_0000L;

    // Strictly between x86-64's TASK_SIZE_MAX (2^47 less a page) and arm64's
    // 2^48, with room for the length at both ends. An arm64 kernel accepts this
    // range and an x86-64 one refuses it, so it is the row that tells the two
    // architectures apart.
    static unsafe byte* BeyondX64 => (byte*)0x0000_9000_0000_0000L;

    static void Dummy() {{ }}

    static unsafe IntPtr OpenPath(string name)
    {{
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }}

    static unsafe bool ReadRejected(IntPtr fd, byte* buffer, int size, int expectedErrno)
    {{
        Marshal.SetLastSystemError(0);
        int r = Read(fd, buffer, size);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }}

    static unsafe bool PReadRejected(IntPtr fd, byte* buffer, int size, long offset, int expectedErrno)
    {{
        Marshal.SetLastSystemError(0);
        int r = PRead(fd, buffer, size, offset);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }}

    static unsafe bool WriteRejected(IntPtr fd, byte* buffer, int size, int expectedErrno)
    {{
        Marshal.SetLastSystemError(0);
        int r = Write(fd, buffer, size);
        return r == -1 && Marshal.GetLastSystemError() == expectedErrno;
    }}

    static unsafe int Main(string[] args)
    {{
{body}
    }}
}}
"""

    let exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
        | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

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

    /// The rows Linux answers with EFAULT and macOS answers by performing the
    /// operation, plus the checks that come *before* the screen on both.
    [<Test>]
    let ``Linux screens the buffer between the access mode and the operation`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        if (LSeek(f, 0, SEEK_END) != 5) return 2;
        IntPtr d = OpenPath("d");
        if (d == new IntPtr(-1)) return 3;

        // Nothing would have moved, and it faults anyway.
        if (!ReadRejected(f, Wild, 5, EFAULT)) return 10;
        if (!ReadRejected(f, Wild, 0, EFAULT)) return 11;
        // Ahead of the file operation, so ahead of EISDIR.
        if (!ReadRejected(d, Wild, 5, EFAULT)) return 12;
        // Ahead of stdin's end-of-file, which is also the file operation.
        if (!ReadRejected(new IntPtr(0), Wild, 5, EFAULT)) return 13;

        // ...but behind the descriptor lookup and the access-mode check, both
        // of which `vfs_read` performs first.
        if (!ReadRejected(new IntPtr(4242), Wild, 5, EBADF)) return 14;
        if (!ReadRejected(new IntPtr(1), Wild, 5, EBADF)) return 15;
        // ...and behind the shim's own guard, which never reaches a kernel.
        if (!ReadRejected(f, Wild, -1, EINVAL)) return 16;

        // Accepted: below TASK_SIZE_MAX, so a user address as far as the screen
        // is concerned, and the read at end-of-file transfers nothing.
        Marshal.SetLastSystemError(0);
        if (Read(f, HighButUser, 5) != 0) return 17;

        // Refused: an arm64 kernel would accept this one.
        if (!ReadRejected(f, BeyondX64, 5, EFAULT)) return 18;

        // pread reaches the same screen through the same `vfs_read`, behind its
        // own seekability check.
        if (!PReadRejected(f, Wild, 5, 5, EFAULT)) return 20;
        if (!PReadRejected(d, Wild, 5, 0, EFAULT)) return 21;
        if (!PReadRejected(new IntPtr(0), Wild, 5, 0, ESPIPE)) return 22;
        if (!PReadRejected(new IntPtr(4242), Wild, 5, 0, EBADF)) return 23;
        Marshal.SetLastSystemError(0);
        if (PRead(f, HighButUser, 5, 5) != 0) return 24;

        // write screens in `vfs_write`, ahead of the zero-size no-op.
        if (!WriteRejected(new IntPtr(1), Wild, 0, EFAULT)) return 30;
        if (!WriteRejected(new IntPtr(1), Wild, 5, EFAULT)) return 31;
        // Behind the access-mode check and the shim's guard.
        if (!WriteRejected(new IntPtr(0), Wild, 5, EBADF)) return 32;
        if (!WriteRejected(new IntPtr(1), Wild, -1, ERANGE)) return 33;

        return 0;
"""

        runOn seed "UserBufferFaultLinux.cs" source |> exitCodeOf |> shouldEqual 0

    /// The same calls on Darwin, which screens nothing up front: each one is
    /// answered by the operation it would have performed. Every row here
    /// differs from its Linux counterpart above, so a model that applied one
    /// platform's rule to both would fail one of the two tests.
    [<Test>]
    let ``Darwin discovers a bad address at the copy`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        if (LSeek(f, 0, SEEK_END) != 5) return 2;
        IntPtr d = OpenPath("d");
        if (d == new IntPtr(-1)) return 3;

        // Nothing moves, so the buffer is never looked at.
        Marshal.SetLastSystemError(0);
        if (Read(f, Wild, 5) != 0) return 10;
        Marshal.SetLastSystemError(0);
        if (Read(f, Wild, 0) != 0) return 11;
        // The operation runs, and answers for the descriptor's kind.
        if (!ReadRejected(d, Wild, 5, EISDIR)) return 12;
        // stdin is at end-of-file, so this transfers nothing either.
        Marshal.SetLastSystemError(0);
        if (Read(new IntPtr(0), Wild, 5) != 0) return 13;

        // Unchanged from Linux: these precede the screen on both platforms.
        if (!ReadRejected(new IntPtr(4242), Wild, 5, EBADF)) return 14;
        if (!ReadRejected(new IntPtr(1), Wild, 5, EBADF)) return 15;
        if (!ReadRejected(f, Wild, -1, EINVAL)) return 16;

        Marshal.SetLastSystemError(0);
        if (Read(f, HighButUser, 5) != 0) return 17;

        // Beyond x86-64's TASK_SIZE_MAX, which macOS has no analogue of: it
        // transfers nothing here rather than refusing, as it does at any
        // address.
        Marshal.SetLastSystemError(0);
        if (Read(f, BeyondX64, 5) != 0) return 18;

        Marshal.SetLastSystemError(0);
        if (PRead(f, Wild, 5, 5) != 0) return 20;
        if (!PReadRejected(d, Wild, 5, 0, EISDIR)) return 21;
        if (!PReadRejected(new IntPtr(0), Wild, 5, 0, ESPIPE)) return 22;
        if (!PReadRejected(new IntPtr(4242), Wild, 5, 0, EBADF)) return 23;

        // A zero-size write is a no-op whatever the pointer is.
        Marshal.SetLastSystemError(0);
        if (Write(new IntPtr(1), Wild, 0) != 0) return 30;
        // A real transfer still faults, at the copy rather than before it.
        if (!WriteRejected(new IntPtr(1), Wild, 5, EFAULT)) return 31;
        if (!WriteRejected(new IntPtr(0), Wild, 5, EBADF)) return 32;
        if (!WriteRejected(new IntPtr(1), Wild, -1, ERANGE)) return 33;

        return 0;
"""

        runOn darwin "UserBufferFaultDarwin.cs" source |> exitCodeOf |> shouldEqual 0

    /// A pointer PawPrint models symbolically — a type handle, a method table —
    /// is a genuine user-space address on a real runtime, so a call that
    /// transfers nothing through it succeeds rather than faulting. The screen
    /// must therefore let it past, on both platforms.
    ///
    /// The screen classifies the buffer ahead of every zero-transfer shortcut,
    /// so these paths reach the classifier even though they never dereference
    /// anything.
    [<Test>]
    let ``a symbolic address transfers nothing without faulting`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        if (LSeek(f, 0, SEEK_END) != 5) return 2;

        byte* handle = (byte*)typeof(int).TypeHandle.Value;
        // A function pointer is the same kind of address, and reaches the
        // classifier by a different route: a cast `ldftn` result stays a native
        // int rather than becoming a runtime pointer.
        delegate*<void> fn = &Dummy;
        byte* code = (byte*)fn;

        // A zero-byte write does not dereference its buffer, and the address is
        // a user address, so there is nothing to refuse.
        Marshal.SetLastSystemError(0);
        if (Write(new IntPtr(1), handle, 0) != 0) return 10;

        // Likewise a read that is already at end-of-file.
        Marshal.SetLastSystemError(0);
        if (Read(f, handle, 5) != 0) return 11;
        Marshal.SetLastSystemError(0);
        if (PRead(f, handle, 5, 5) != 0) return 12;

        Marshal.SetLastSystemError(0);
        if (Write(new IntPtr(1), code, 0) != 0) return 13;
        Marshal.SetLastSystemError(0);
        if (Read(f, code, 5) != 0) return 14;

        return 0;
"""

        for kernel, name in [ seed, "SymbolicBufferLinux.cs" ; darwin, "SymbolicBufferDarwin.cs" ] do
            runOn kernel name source |> exitCodeOf |> shouldEqual 0

    /// The refusal for a symbolic address fires at the *transfer*, and every
    /// check that precedes the transfer still answers.
    ///
    /// These four rows are what pins the refusal's position. A
    /// classification-time refusal — refusing as soon as the argument is seen to
    /// be symbolic — passes every other row in this fixture, and turns each of
    /// these four from an answer into a crash.
    [<Test>]
    let ``every check before the transfer still answers a symbolic address`` () : unit =
        let source =
            guest
                """
        byte* handle = (byte*)typeof(int).TypeHandle.Value;

        // The descriptor is checked before the buffer is looked at at all.
        Marshal.SetLastSystemError(0);
        if (!ReadRejected(new IntPtr(999), handle, 5, EBADF)) return 10;

        // Stdin is the read end of a closed pipe, so this is end-of-file, and a
        // read that returns end-of-file never touches its buffer.
        Marshal.SetLastSystemError(0);
        if (Read(new IntPtr(0), handle, 5) != 0) return 11;

        // A directory is EISDIR, decided from the inode and not from the buffer.
        IntPtr d = OpenPath("d");
        if (d == new IntPtr(-1)) return 12;
        if (!ReadRejected(d, handle, 5, EISDIR)) return 13;

        // And on the write side, the access mode precedes the buffer: `f` is
        // opened O_RDONLY.
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 14;
        if (!WriteRejected(f, handle, 5, EBADF)) return 15;

        return 0;
"""

        for kernel, name in
            [
                seed, "SymbolicBufferPrecedenceLinux.cs"
                darwin, "SymbolicBufferPrecedenceDarwin.cs"
            ] do
            runOn kernel name source |> exitCodeOf |> shouldEqual 0

    /// ...but a transfer *through* one is refused rather than answered. EFAULT
    /// would be a wrong answer: a real kernel reads the bytes of the runtime
    /// structure at that address quite happily, and PawPrint has no bytes there
    /// to give.
    [<Test>]
    let ``a transfer through a symbolic address is refused`` () : unit =
        let source =
            guest
                """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;

        // Offset 0 of a five-byte file, so this really would move bytes.
        PRead(f, (byte*)typeof(int).TypeHandle.Value, 5, 0);
        return 0;
"""

        let exn =
            Assert.Catch (fun () -> runOn seed "SymbolicBufferTransfer.cs" source |> ignore<RunOutcome>)

        exn.Message |> shouldContainText "SystemNative_PRead"
        exn.Message |> shouldContainText "models symbolically"
