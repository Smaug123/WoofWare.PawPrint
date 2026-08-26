namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `cpblk` (ECMA-335 III.3.30) against the real runtime, over the operand shapes and endpoints no
/// C# source can spell.
///
/// `sourcesPure/StackallocNonUniformInit.cs` covers the one route Roslyn does emit — a `stackalloc`
/// whose initializer is not byte-uniform — but that route fixes almost everything about the
/// instruction: the destination is always a freshly-`localloc`'d flat byte pool, the source is
/// always a static RVA blob, and the count always exactly spans the block. What is left over is
/// what this fixture fabricates: endpoints that are *typed cells* rather than byte pools, a count
/// that stops short of the end of its storage, a null endpoint on either side, and a range whose
/// cells have no byte image at all.
///
/// Overlapping ranges are deliberately absent. III.3.30 leaves the result unspecified when source
/// and destination overlap, so the real runtime cannot be an oracle for them.
///
/// The fabricated assembly is two methods that are literally
/// `ldarg.0; ldarg.1; ldarg.2; cpblk; ret`, one over `ref byte` and one over `byte*`, so the driver
/// can put any operands it likes in front of the instruction. `expectedOnHost` is asserted too, so
/// a fabrication that stopped exercising the shape fails here instead of passing vacuously.
[<TestFixture>]
module TestFabricatedCpblk =

    /// `Cp::Copy(ref byte, ref byte, uint)` and `Cp::CopyPtr(byte*, byte*, uint)`, each a bare
    /// `cpblk` over its three arguments.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Cp", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Cp"

        let cp =
            modul.DefineType ("Cp", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let body (method : MethodBuilder) : unit =
            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Ldarg_2
            il.Emit OpCodes.Cpblk
            il.Emit OpCodes.Ret

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        body (
            cp.DefineMethod (
                "Copy",
                attributes,
                typeof<Void>,
                [|
                    typeof<byte>.MakeByRefType ()
                    typeof<byte>.MakeByRefType ()
                    typeof<uint32>
                |]
            )
        )

        body (
            cp.DefineMethod (
                "CopyPtr",
                attributes,
                typeof<Void>,
                [|
                    typeof<byte>.MakePointerType ()
                    typeof<byte>.MakePointerType ()
                    typeof<uint32>
                |]
            )
        )

        cp.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own index on the first check it fails, and 0 when every check
    /// passes, so a disagreement names the scenario. A packed bitmask would not fit: a process's
    /// exit code on Unix is eight bits and `128 + signo` reports a signalled child, so eight
    /// independent bits cannot be distinguished from a crash.
    let private driverSource : string =
        """
using System;
using System.Runtime.CompilerServices;

public static class Driver
{
    // Both endpoints are byrefs into the middle of a byte[], which is a *typed cell* root rather
    // than the flat byte pool a stackalloc gives.
    private static int ByrefIntoArray()
    {
        byte[] src = new byte[8] { 1, 2, 3, 4, 5, 6, 7, 8 };
        byte[] dst = new byte[8];
        Cp.Copy(ref dst[2], ref src[5], 3);

        if (dst[0] != 0 || dst[1] != 0) return 1;
        if (dst[2] != 6 || dst[3] != 7 || dst[4] != 8) return 2;
        if (dst[5] != 0 || dst[6] != 0 || dst[7] != 0) return 3;
        return 0;
    }

    // The same range through the pointer operand shape instead of the byref one.
    private static unsafe int PointerIntoArray()
    {
        byte[] src = new byte[8] { 1, 2, 3, 4, 5, 6, 7, 8 };
        byte[] dst = new byte[8];
        fixed (byte* s = src)
        fixed (byte* d = dst)
        {
            Cp.CopyPtr(d + 1, s + 4, 3);
        }

        if (dst[0] != 0) return 4;
        if (dst[1] != 5 || dst[2] != 6 || dst[3] != 7) return 5;
        if (dst[4] != 0) return 6;
        return 0;
    }

    // A zero count copies nothing at all, and must not read or write either endpoint.
    private static int ZeroCountCopiesNothing()
    {
        byte[] src = new byte[2] { 9, 9 };
        byte[] dst = new byte[2] { 5, 6 };
        Cp.Copy(ref dst[0], ref src[0], 0);
        if (dst[0] != 5 || dst[1] != 6) return 7;
        return 0;
    }

    // A null endpoint with a zero count is legal on either side and must not fault.
    private static unsafe int NullWithZeroCount()
    {
        byte[] arr = new byte[1];
        try
        {
            Cp.CopyPtr(null, null, 0);
            fixed (byte* p = arr)
            {
                Cp.CopyPtr(null, p, 0);
                Cp.CopyPtr(p, null, 0);
            }
        }
        catch (NullReferenceException)
        {
            return 8;
        }

        return 0;
    }

    // A null source with a nonzero count raises a NullReferenceException the guest can catch,
    // rather than killing the process.
    private static unsafe int NullSourceWithNonzeroCount()
    {
        byte[] arr = new byte[4];
        try
        {
            fixed (byte* p = arr)
            {
                Cp.CopyPtr(p, null, 1);
            }
            return 9;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // And the same on the destination side.
    private static unsafe int NullDestinationWithNonzeroCount()
    {
        byte[] arr = new byte[4] { 1, 2, 3, 4 };
        try
        {
            fixed (byte* p = arr)
            {
                Cp.CopyPtr(null, p, 1);
            }
            return 10;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A count in bytes over int cells: byte-addressable, so it goes down the byte walk, but under
    // a typed-cell root rather than a byte pool. Eight bytes covers exactly two elements, so a
    // count read as an element count would copy two bytes and stop.
    private static int CountsBytesOverIntCells()
    {
        int[] src = new int[4] { 11, 22, 33, 44 };
        int[] dst = new int[4];
        Cp.Copy(ref Unsafe.As<int, byte>(ref dst[1]), ref Unsafe.As<int, byte>(ref src[2]), 8);

        if (dst[0] != 0) return 11;
        if (dst[1] != 33 || dst[2] != 44) return 12;
        if (dst[3] != 0) return 13;
        return 0;
    }

    // Copying a null object reference over another. This is the row that makes the whole-cell path
    // load-bearing: an object reference has no byte image to read or write, so only a step that
    // moves the cell itself can serve it. It is a lawful operation on the real runtime — writing
    // null needs no GC write barrier — so the oracle can run it. Copying a *non-null* reference
    // this way would be genuine heap corruption, has no runnable oracle, and is not attempted.
    private static int NullReferenceOverReference()
    {
        object[] src = new object[1];
        object[] dst = new object[3];
        object keepFirst = new object();
        object keepLast = new object();
        dst[0] = keepFirst;
        dst[1] = new object();
        dst[2] = keepLast;

        Cp.Copy(ref Unsafe.As<object, byte>(ref dst[1]), ref Unsafe.As<object, byte>(ref src[0]), (uint)IntPtr.Size);

        if (!ReferenceEquals(dst[0], keepFirst)) return 14;
        if (dst[1] != null) return 15;
        if (!ReferenceEquals(dst[2], keepLast)) return 16;
        return 0;
    }

    public static int Main(string[] args)
    {
        int r = ByrefIntoArray();
        if (r != 0) return r;
        r = PointerIntoArray();
        if (r != 0) return r;
        r = ZeroCountCopiesNothing();
        if (r != 0) return r;
        r = NullWithZeroCount();
        if (r != 0) return r;
        r = NullSourceWithNonzeroCount();
        if (r != 0) return r;
        r = NullDestinationWithNonzeroCount();
        if (r != 0) return r;
        r = CountsBytesOverIntCells();
        if (r != 0) return r;
        r = NullReferenceOverReference();
        if (r != 0) return r;
        return 0;
    }
}
"""

    [<Test>]
    let ``cpblk agrees with the real runtime on operands C# cannot spell`` () : unit =
        FabricatedGuest.run "Cp" (fabricate ()) "CpblkDriver" driverSource 0
