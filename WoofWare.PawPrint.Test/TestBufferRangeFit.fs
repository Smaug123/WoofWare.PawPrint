namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// What a `SystemNative_*` transfer does when its bytes would run past the end
/// of the storage its buffer names.
///
/// Not the same question as the address screen in `TestUserBufferFault`, and
/// not one a kernel asks: `access_ok` bounds a range against the *address
/// space*, never against the guest's own allocation, so a real kernel serves an
/// over-long transfer by touching whatever follows the buffer. PawPrint's
/// address space is a graph of typed cells with nothing following anything, so
/// it refuses and names the syscall.
///
/// Every storage kind a buffer can name is exercised twice: once with transfers
/// that exactly fill it, which is what would break if an extent were computed a
/// byte short, and once with one that cannot.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBufferRangeFit =

    let private assy = System.Reflection.Assembly.GetExecutingAssembly ()

    /// A file of 32 bytes, so that a transfer bounded by the file is still
    /// larger than any buffer below.
    let private seed : KernelConfig =
        { KernelConfig.Default with
            FileSystem =
                Map.ofList
                    [
                        DirectoryEntryName.parseOrFail "test seed" "f",
                        SeedEntry.file (
                            System.Text.Encoding.UTF8.GetBytes (System.String ('x', 32))
                            |> ImmutableArray.CreateRange
                        )
                    ]
        }

    /// Raw P/Invokes: the BCL's own wrappers pass real spans, so no managed call
    /// site can hand a syscall a buffer smaller than the count beside it.
    let private guest (body : string) : string =
        $"""
using System;
using System.Runtime.InteropServices;

struct Quad {{ public byte A; public byte B; public byte C; public byte D; }}

class Boxed {{ public byte A; public byte B; public byte C; public byte D; }}

class Program
{{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Open", SetLastError = true)]
    static extern unsafe IntPtr Open(byte* path, int flags, int mode);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PRead", SetLastError = true)]
    static extern unsafe int PRead(IntPtr fd, byte* buffer, int bufferSize, long offset);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_Write", SetLastError = true)]
    static extern unsafe int Write(IntPtr fd, byte* buffer, int bufferSize);

    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetNonCryptographicallySecureRandomBytes")]
    static extern unsafe void RandomBytes(byte* buffer, int bufferLength);

    static Quad Static;

    static unsafe IntPtr OpenPath(string name)
    {{
        byte* path = stackalloc byte[16];
        for (int i = 0; i < name.Length; i++) path[i] = (byte)name[i];
        path[name.Length] = 0;
        return Open(path, 0, 0);
    }}

    /// Both directions through a buffer of exactly `capacity` bytes, at every
    /// length up to and including that capacity.
    static unsafe bool Fills(IntPtr f, byte* buffer, int capacity)
    {{
        for (int n = 0; n <= capacity; n++)
        {{
            if (PRead(f, buffer, n, 0) != n) return false;
            if (Write(1, buffer, n) != n) return false;
        }}
        return true;
    }}

    /// Only the outbound direction, for storage a guest cannot write to.
    static unsafe bool Drains(byte* buffer, int capacity)
    {{
        for (int n = 0; n <= capacity; n++)
        {{
            if (Write(1, buffer, n) != n) return false;
        }}
        return true;
    }}

    static unsafe bool ThroughArgument(IntPtr f, Quad argument)
    {{
        return Fills(f, &argument.A, 4);
    }}

    static unsafe void OverrunArgument(IntPtr f, Quad argument)
    {{
        PRead(f, &argument.A, 100, 0);
    }}

    static unsafe int Main(string[] args)
    {{
{body}
    }}
}}
"""

    let private exitCodeOf (outcome : RunOutcome) : int =
        match outcome with
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) -> state.LatchedExitCode
        | other -> failwith $"expected the guest to terminate cleanly, got %O{other}"

    let private run (name : string) (body : string) : RunOutcome =
        let image = Roslyn.compile [ guest body ]

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

    /// Transfers that exactly fill their buffer, for every storage kind a buffer
    /// can name. An extent computed a byte short would fail the last iteration
    /// of each loop.
    [<Test>]
    let ``a transfer that exactly fills its buffer is served`` () : unit =
        let body =
            """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;

        byte* block = stackalloc byte[8];
        for (int i = 0; i < 8; i++) block[i] = 0;
        if (!Fills(f, block, 8)) return 10;

        int[] array = new int[2];
        fixed (int* p = array) { if (!Fills(f, (byte*)p, 8)) return 11; }

        byte local = 0;
        if (!Fills(f, &local, 1)) return 12;

        if (!ThroughArgument(f, default)) return 13;

        fixed (byte* p = &Static.A) { if (!Fills(f, p, 4)) return 14; }

        byte* heap = (byte*)NativeMemory.AllocZeroed(8);
        if (!Fills(f, heap, 8)) return 15;
        NativeMemory.Free(heap);

        Boxed boxed = new Boxed();
        fixed (byte* p = &boxed.A) { if (!Fills(f, p, 4)) return 16; }

        // A string's characters plus the null terminator CoreCLR's string
        // layout reserves, which is addressable and so part of the storage.
        fixed (char* p = "hi") { if (!Drains((byte*)p, 6)) return 17; }

        // A UTF-8 literal is static PE data rather than a heap allocation.
        fixed (byte* p = "abcd"u8) { if (!Drains(p, 4)) return 18; }

        return 0;
"""

        run "BufferRangeFits.cs" body |> exitCodeOf |> shouldEqual 0

    /// What is required to fit is what actually moves, not what was asked for.
    /// A read bounded by the end of the file transfers less than the count
    /// beside it, and a kernel copies only what it transfers — so a request
    /// larger than the buffer is served, provided the transfer is not.
    [<Test>]
    let ``only the bytes that move have to fit`` () : unit =
        let body =
            """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;

        byte* block = stackalloc byte[4];
        for (int i = 0; i < 4; i++) block[i] = 0;

        // Four bytes remain in the file, and four is what the buffer holds.
        if (PRead(f, block, 100, 28) != 4) return 10;
        // Nothing remains, so nothing is copied and the buffer is untouched.
        if (PRead(f, block, 100, 32) != 0) return 11;

        return 0;
"""

        run "BufferRangeTransferOnly.cs" body |> exitCodeOf |> shouldEqual 0

    /// A buffer whose coordinate within its storage cannot be derived is let
    /// past rather than refused: only an overrun that has actually been
    /// established is worth reporting. `(byte*)123` names no storage at all, so
    /// it fails where it always did — at the write, not at the range check.
    [<Test>]
    let ``a buffer naming no storage is not reported as an overrun`` () : unit =
        let body =
            """
        byte* wild = (byte*)123;
        RandomBytes(wild, 4);
        return 0;
"""

        let exn =
            Assert.Catch (fun () -> run "BufferRangeUnresolvable.cs" body |> ignore<RunOutcome>)

        if exn.Message.Contains "leaves the storage the buffer names" then
            failwith
                $"the range check reported an overrun for a pointer that names no storage, so it is refusing what it has not established: %s{exn.Message}"

    /// One row per storage kind: the transfer cannot fit, and the refusal names
    /// the syscall, the storage, and both numbers.
    ///
    /// The counts are far larger than any buffer here, so a storage whose exact
    /// size depends on padding is still unambiguously overrun; the two rows
    /// whose extent is fixed by the allocation itself assert that number.
    let private overrunCases : obj array seq =
        seq {
            yield
                [|
                    "BufferRangeOverrunStackMemory.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        byte* block = stackalloc byte[8];
        PRead(f, block, 100, 0);
        return 0;
"""
                    // The read is bounded by the 32-byte file, so the count
                    // reported is the transfer rather than the request.
                    [
                        "SystemNative_PRead"
                        "would write 32 bytes"
                        "stack memory block"
                        "spans 8 bytes"
                    ]
                |]

            yield
                [|
                    "BufferRangeOverrunArray.cs"
                    """
        int[] array = new int[2];
        fixed (int* p = array) { Write(1, (byte*)p, 100); }
        return 0;
"""
                    [
                        "SystemNative_Write"
                        "would read 100 bytes"
                        "storage of array"
                        "spans 8 bytes"
                    ]
                |]

            yield
                [|
                    "BufferRangeOverrunLocal.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        byte local = 0;
        PRead(f, &local, 100, 0);
        return 0;
"""
                    [ "SystemNative_PRead" ; "variable" ; "spans 1 byte" ]
                |]

            yield
                [|
                    "BufferRangeOverrunArgument.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        OverrunArgument(f, default);
        return 0;
"""
                    [ "SystemNative_PRead" ; "argument" ; "spans 4 bytes" ]
                |]

            yield
                [|
                    "BufferRangeOverrunStaticField.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        fixed (byte* p = &Static.A) { PRead(f, p, 100, 0); }
        return 0;
"""
                    [ "SystemNative_PRead" ; "static field" ; "spans 4 bytes" ]
                |]

            yield
                [|
                    "BufferRangeOverrunNativeMemory.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        byte* heap = (byte*)NativeMemory.AllocZeroed(8);
        PRead(f, heap, 100, 0);
        return 0;
"""
                    [ "SystemNative_PRead" ; "native memory block" ; "spans 8 bytes" ]
                |]

            yield
                [|
                    "BufferRangeOverrunHeapObject.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        Boxed boxed = new Boxed();
        fixed (byte* p = &boxed.A) { PRead(f, p, 100, 0); }
        return 0;
"""
                    [ "SystemNative_PRead" ; "storage of heap object" ; "spans 4 bytes" ]
                |]

            yield
                [|
                    "BufferRangeOverrunString.cs"
                    """
        fixed (char* p = "hi") { Write(1, (byte*)p, 100); }
        return 0;
"""
                    [ "SystemNative_Write" ; "character storage of string" ; "spans 6 bytes" ]
                |]

            // The other end of the storage. A buffer displaced before the thing
            // it points into fits by arithmetic — one byte short of eight still
            // leaves room for four — and is out of bounds all the same.
            yield
                [|
                    "BufferRangeOverrunBelow.cs"
                    """
        IntPtr f = OpenPath("f");
        if (f == new IntPtr(-1)) return 1;
        int[] array = new int[2];
        fixed (int* p = array) { PRead(f, (byte*)p - 1, 4, 0); }
        return 0;
"""
                    [ "SystemNative_PRead" ; "starting at byte -1" ; "storage of array" ]
                |]
        }

    [<TestCaseSource(nameof overrunCases)>]
    let ``a transfer that cannot fit its buffer is refused`` (name : string) (body : string) (expected : string list) =
        let exn = Assert.Catch (fun () -> run name body |> ignore<RunOutcome>)

        for fragment in expected do
            exn.Message |> shouldContainText fragment
