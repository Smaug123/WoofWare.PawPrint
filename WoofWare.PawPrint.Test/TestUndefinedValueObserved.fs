namespace WoofWare.Pawprint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// Guests that *use* a value read from memory nothing wrote: branch on it, compute with it,
/// return it as the exit code, hand it to the runtime. Real .NET's behaviour is undefined for
/// every one of them, so none can be checked against the real runtime; what is checked instead is
/// that PawPrint ends the run with `RunOutcome.UndefinedValueObserved`, naming the use and the
/// never-written bytes, rather than inventing a value or crashing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestUndefinedValueObserved =

    /// The observation the run ended with, failing on any other outcome.
    let private observed (outcome : RunOutcome) : UndefinedValueObservation =
        match outcome with
        | RunOutcome.UndefinedValueObserved (_, _, observation) -> observation
        | RunOutcome.NormalExit (state, _) ->
            failwith
                $"expected an undefined value to be observed, got a normal exit with code %d{state.LatchedExitCode}"
        | RunOutcome.ProcessExit _ -> failwith "expected an undefined value to be observed, got a process exit"
        | RunOutcome.Aborted (_, _, fatal) ->
            failwith $"expected an undefined value to be observed, got an abort (%O{fatal.Code}): %A{fatal.Message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"expected an undefined value to be observed, got signal termination %O{signal}"
        | RunOutcome.GuestUnhandledException (state, _, exn) ->
            failwith
                $"expected an undefined value to be observed, got an unhandled exception:\n%s{UnhandledExceptionReport.describe state exn}"

    /// The offsets of the never-written bytes `value` descends from, each of which must be a
    /// byte of a `localloc` block.
    let private stackOrigins (value : UndefinedValue) : int list =
        value.Origins
        |> List.map (fun origin ->
            match origin.Memory with
            | UninitialisedMemory.Stack _ -> origin.Offset
            | UninitialisedMemory.Native block -> failwith $"expected a localloc origin, got %O{block}"
        )

    let private run (name : string) (source : string) (check : UndefinedValueObservation -> unit) : unit =
        TestPureCases.runPawPrintSource name source KernelConfig.Default (fun _ outcome -> check (observed outcome))

    [<Test>]
    let ``Branching on an unwritten stackalloc bool ends the run at the branch`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        bool* flags = stackalloc bool[4];
        flags[0] = true;
        // `MethodBaseInvoker.CopyBack`'s shape: a flag nothing set, branched on.
        if (flags[2]) return 1;
        return 0;
    }
}
"""

        run
            "UndefinedBranch.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 2 ]

                match observation.Use with
                | UndefinedValueUse.Operand (method, _, instruction, fromTop) ->
                    method.Name |> shouldEqual "Main"
                    fromTop |> shouldEqual 0

                    match instruction with
                    | IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brfalse _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _) -> ()
                    | other -> failwith $"expected a conditional branch, got %O{other}"
                | other -> failwith $"expected an instruction operand, got %O{other}"
            )

    [<Test>]
    let ``Arithmetic on an unwritten stackalloc int ends the run at the arithmetic`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[2];
        numbers[1] = 5;
        // Pushed first, so the undefined operand is the one beneath the top.
        int sum = numbers[0] + numbers[1];
        return sum == 5 ? 0 : 1;
    }
}
"""

        run
            "UndefinedArithmetic.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                match observation.Use with
                | UndefinedValueUse.Operand (_, _, IlOp.Nullary NullaryIlOp.Add, fromTop) -> fromTop |> shouldEqual 1
                | other -> failwith $"expected the add's operand, got %O{other}"
            )

    [<Test>]
    let ``A partly written value is undefined if any byte of it is`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 1;
        bytes[1] = 2;
        bytes[3] = 4;
        // Three of the four bytes are written; the int they make up is still undefined.
        int whole = *(int*)bytes;
        return whole > 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedPartlyWritten.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 2 ]

                observation.Value.Bytes
                |> shouldEqual
                    [
                        ValueByte.Defined 1uy
                        ValueByte.Defined 2uy
                        observation.Value.Bytes.[2]
                        ValueByte.Defined 4uy
                    ]
            )

    [<Test>]
    let ``Returning an unwritten value from Main ends the run at the exit code`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        return numbers[0];
    }
}
"""

        run
            "UndefinedExitCode.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32

                match observation.Use with
                | UndefinedValueUse.ExitCode -> ()
                | other -> failwith $"expected the exit code, got %O{other}"
            )

    [<Test>]
    let ``Passing an unwritten value to a runtime-implemented method ends the run at the call`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        // Moved through an argument into the managed wrapper, then handed to the runtime.
        Environment.Exit(numbers[0]);
        return 0;
    }
}
"""

        run
            "UndefinedRuntimeArgument.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32

                match observation.Use with
                | UndefinedValueUse.RuntimeArgument (_, index) -> index |> shouldEqual 0
                | other -> failwith $"expected a runtime-implemented method's argument, got %O{other}"
            )

    [<Test>]
    let ``Boxing a Nullable whose hasValue nothing wrote ends the run at the box`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        // `byte?` has no padding, so each of its two bytes is a field's.
        byte?* slots = stackalloc byte?[1];
        // Copying the Nullable moves its undefined fields; boxing it reads hasValue.
        byte? copy = slots[0];
        object boxed = copy;
        return boxed is null ? 0 : 1;
    }
}
"""

        run
            "UndefinedNullableBox.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]

                match observation.Use with
                | UndefinedValueUse.InstructionDetail (_, _, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, _), _) ->
                    ()
                | other -> failwith $"expected the box's read of hasValue, got %O{other}"
            )

    [<Test>]
    let ``A constrained call that would box a Nullable whose hasValue nothing wrote ends the run at the call``
        ()
        : unit
        =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    // `value.GetType()` on a `ref T` is `constrained. !!T callvirt Object::GetType`, which boxes
    // the Nullable it points at, and so reads its hasValue.
    static Type TypeOf<T>(ref T value) => value.GetType();

    static int Main(string[] args)
    {
        byte?* slots = stackalloc byte?[1];
        byte? copy = slots[0];
        return TypeOf(ref copy) == typeof(byte) ? 0 : 1;
    }
}
"""

        run
            "UndefinedNullableConstrained.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]

                match observation.Use with
                | UndefinedValueUse.InstructionDetail (method,
                                                       _,
                                                       IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _),
                                                       _) -> method.Name |> shouldEqual "TypeOf"
                | other -> failwith $"expected the constrained callvirt's read of hasValue, got %O{other}"
            )

    /// The observation must be the runtime's read through a pointer, of the undefined value
    /// `expected` names; `method` names the runtime-implemented method that read it.
    /// The observation is `methodName`'s read of `what`. A P/Invoke's own name is generated, so
    /// `methodName` is a prefix of it.
    let private expectReadByRuntime
        (methodName : string)
        (what : string)
        (observation : UndefinedValueObservation)
        : unit
        =
        match observation.Use with
        | UndefinedValueUse.ReadByRuntime (method, actualWhat) ->
            method.Name.StartsWith (methodName, StringComparison.Ordinal)
            |> shouldEqual true

            actualWhat |> shouldEqual what
        | other -> failwith $"expected %s{methodName}'s read of %s{what}, got %O{other}"

    [<Test>]
    let ``Interlocked.Add on a location holding an undefined value ends the run at the add`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        int location = numbers[0];
        // Only moved so far; the add reads it.
        return Interlocked.Add(ref location, 1) == 1 ? 0 : 1;
    }
}
"""

        run
            "UndefinedInterlockedAdd.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]
                expectReadByRuntime "ExchangeAdd" "the location it operates on" observation
            )

    [<Test>]
    let ``Interlocked.CompareExchange on a location holding an undefined value ends the run at the compare`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        long* numbers = stackalloc long[1];
        long location = numbers[0];
        return Interlocked.CompareExchange(ref location, 1, 0) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedInterlockedCompareExchange.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int64
                expectReadByRuntime "CompareExchange" "the location it operates on" observation
            )

    [<Test>]
    let ``Writing a buffer with an unwritten byte to a stream ends the run at the write`` () : unit =
        let source =
            """
using System;
using System.IO;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 65;
        bytes[1] = 66;
        bytes[3] = 10;
        using Stream output = Console.OpenStandardOutput();
        // The native write reads all four bytes, byte 2 among them.
        output.Write(new ReadOnlySpan<byte>(bytes, 4));
        return 0;
    }
}
"""

        run
            "UndefinedNativeWrite.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 2 ]
                expectReadByRuntime "<Write>g__" "the buffer it writes out" observation
            )

    [<Test>]
    let ``A path with an unwritten byte before its terminator ends the run at the syscall`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern int MkDir(byte* path, int mode);

    static int Main(string[] args)
    {
        byte* existing = stackalloc byte[2];
        existing[0] = (byte)'e';
        existing[1] = 0;
        // The second fails with EEXIST, which is then the thread's last error.
        if (MkDir(existing, 0x1ff) != 0) return 2;
        if (MkDir(existing, 0x1ff) == 0) return 3;

        byte* path = stackalloc byte[3];
        path[0] = (byte)'d';
        path[2] = 0;
        // The scan for the terminator compares byte 1 with NUL.
        return MkDir(path, 0x1ff) == 0 ? 0 : 1;
    }
}
"""

        TestPureCases.runPawPrintSource
            "UndefinedNativePath.cs"
            source
            KernelConfig.Default
            (fun _ outcome ->
                match outcome with
                | RunOutcome.UndefinedValueObserved (state, thread, _) ->
                    // Reported at the state from before the call, so the P/Invoke's clearing of
                    // the last error on entry is not part of it.
                    EmulatedKernel.lastSystemErrorFor thread state.Kernel |> shouldEqual 17
                | _ -> ()

                let observation = observed outcome
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 1 ]
                expectReadByRuntime "MkDir" "the path it reads" observation
            )

    [<Test>]
    let ``A socket address with an unwritten port ends the run at the port's read`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    static int Main(string[] args)
    {
        byte* address = stackalloc byte[16];
        // `sa_family = AF_INET` in Linux's two-byte layout; the port, at bytes 2 and 3, is
        // never written.
        address[0] = 2;
        address[1] = 0;
        ushort port;
        return GetPort(address, 16, &port) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedNativeSockaddrPort.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 2 ]
                expectReadByRuntime "GetPort" "the socket address's port" observation
            )

    [<Test>]
    let ``A socket address with an unwritten family ends the run at the family's read`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    static int Main(string[] args)
    {
        byte* address = stackalloc byte[16];
        // Only the port is written; the family, at bytes 0 and 1 on Linux, is not.
        address[2] = 0;
        address[3] = 80;
        ushort port;
        return GetPort(address, 16, &port) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedNativeSockaddrFamily.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 0 ]
                expectReadByRuntime "GetPort" "the socket address's family" observation
            )

    [<Test>]
    let ``Comparing spans a byte of which is unwritten ends the run at the comparison`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* left = stackalloc byte[3];
        left[0] = 1;
        left[2] = 3;
        byte* right = stackalloc byte[3];
        right[0] = 1;
        right[1] = 2;
        right[2] = 3;
        // Byte 0 is equal, so the comparison goes on to byte 1 of `left`.
        return new ReadOnlySpan<byte>(left, 3).SequenceEqual(new ReadOnlySpan<byte>(right, 3)) ? 0 : 1;
    }
}
"""

        run
            "UndefinedSequenceEqual.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 1 ]
                expectReadByRuntime "SequenceEqual" "the bytes it compares" observation
            )

    [<Test>]
    let ``Making a string of a span of chars one of which is unwritten ends the run there`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        char* chars = stackalloc char[3];
        chars[0] = 'a';
        chars[2] = 'c';
        return new ReadOnlySpan<char>(chars, 3).ToString().Length == 3 ? 0 : 1;
    }
}
"""

        run
            "UndefinedSpanToString.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Char
                stackOrigins observation.Value |> shouldEqual [ 2 ; 3 ]
                expectReadByRuntime "ToString" "the characters of the span" observation
            )

    [<Test>]
    let ``Comparing char spans one of which holds an unwritten char ends the run at the comparison`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        char* chars = stackalloc char[2];
        chars[0] = 'a';
        return new ReadOnlySpan<char>(chars, 2).Equals("ab".AsSpan(), StringComparison.Ordinal) ? 0 : 1;
    }
}
"""

        run
            "UndefinedMemoryExtensionsEquals.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Char
                stackOrigins observation.Value |> shouldEqual [ 2 ; 3 ]
                expectReadByRuntime "Equals" "the characters of the spans" observation
            )
