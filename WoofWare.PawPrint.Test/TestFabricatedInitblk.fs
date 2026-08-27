namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `initblk` (ECMA-335 III.3.36) against the real runtime, over the operand shapes and
/// destinations no C# source can spell.
///
/// `sourcesPure/StackallocRepeatedInit.cs` covers the one route Roslyn does emit — a byte-uniform
/// `stackalloc` initializer — but that route fixes almost everything about the instruction: the
/// destination is always a freshly-`localloc`'d flat byte pool, the fill value is always a `byte`
/// constant, and the size always exactly spans the block. What is left over is what this fixture
/// fabricates: a destination that is a *typed cell* rather than a byte pool, a fill value carrying
/// bits above the low eight, a size that stops short of the end of its storage, and a null
/// address.
///
/// The fabricated assembly is two methods that are literally
/// `ldarg.0; ldarg.1; ldarg.2; initblk; ret`, one over `ref byte` and one over `byte*`, so the
/// driver can put any operands it likes in front of the instruction. The real runtime is the
/// oracle for every row rather than a remembered number: `expectedOnHost` is asserted too, so a
/// fabrication that stopped exercising the shape fails here instead of passing vacuously.
[<TestFixture>]
module TestFabricatedInitblk =

    /// `Blk::Init(ref byte, int, uint)` and `Blk::InitPtr(byte*, int, uint)`, each a bare
    /// `initblk` over its three arguments.
    ///
    /// The `value` parameter is `int`, not `byte`, which is the point of fabricating it at all:
    /// the instruction takes an unsigned int8 widened to int32 on the stack and writes only its
    /// low eight bits, and a C# signature of `byte` would truncate at the call site instead, where
    /// the interpreter never sees it.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Blk", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Blk"

        let blk =
            modul.DefineType ("Blk", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let body (method : MethodBuilder) : unit =
            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Ldarg_2
            il.Emit OpCodes.Initblk
            il.Emit OpCodes.Ret

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        body (
            blk.DefineMethod (
                "Init",
                attributes,
                typeof<Void>,
                [| typeof<byte>.MakeByRefType () ; typeof<int> ; typeof<uint32> |]
            )
        )

        body (
            blk.DefineMethod (
                "InitPtr",
                attributes,
                typeof<Void>,
                [| typeof<byte>.MakePointerType () ; typeof<int> ; typeof<uint32> |]
            )
        )

        blk.CreateType () |> ignore<Type>

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
    // A byref into the middle of a byte[], which is a *typed cell* root rather than the flat byte
    // pool a stackalloc gives: PawPrint reaches it through array-element cells.
    private static int ByrefIntoArray()
    {
        byte[] arr = new byte[8];
        Blk.Init(ref arr[2], 0xAB, 3);

        if (arr[0] != 0 || arr[1] != 0) return 1;
        if (arr[2] != 0xAB || arr[3] != 0xAB || arr[4] != 0xAB) return 2;
        if (arr[5] != 0 || arr[6] != 0 || arr[7] != 0) return 3;
        return 0;
    }

    // The same range through the pointer operand shape instead of the byref one.
    private static unsafe int PointerIntoArray()
    {
        byte[] arr = new byte[8];
        fixed (byte* p = arr)
        {
            Blk.InitPtr(p + 2, 0xCD, 3);
        }

        if (arr[1] != 0) return 4;
        if (arr[2] != 0xCD || arr[3] != 0xCD || arr[4] != 0xCD) return 5;
        if (arr[5] != 0) return 6;
        return 0;
    }

    // Only the low eight bits of `value` are written. C# cannot express this: its own `stackalloc`
    // initializers carry a `byte` constant.
    private static int ValueTruncatesToLowByte()
    {
        byte[] arr = new byte[4];
        Blk.Init(ref arr[0], 0x1FF, 2);
        if (arr[0] != 0xFF || arr[1] != 0xFF) return 7;
        if (arr[2] != 0 || arr[3] != 0) return 8;

        // Negative values reach the instruction as an int32 too, and are truncated the same way.
        Blk.Init(ref arr[2], -1, 1);
        if (arr[2] != 0xFF) return 9;
        if (arr[3] != 0) return 10;
        return 0;
    }

    // A zero size writes nothing at all.
    private static int ZeroCountWritesNothing()
    {
        byte[] arr = new byte[2];
        arr[0] = 5;
        arr[1] = 6;
        Blk.Init(ref arr[0], 0x77, 0);
        if (arr[0] != 5 || arr[1] != 6) return 11;
        return 0;
    }

    // A null address with a zero size is legal and must not fault.
    private static unsafe int NullWithZeroCount()
    {
        try
        {
            Blk.InitPtr(null, 0, 0);
        }
        catch (NullReferenceException)
        {
            return 12;
        }

        return 0;
    }

    // A null address with a nonzero size raises a NullReferenceException the guest can catch,
    // rather than killing the process.
    private static unsafe int NullWithNonzeroCount()
    {
        try
        {
            Blk.InitPtr(null, 0, 1);
            return 13;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A nonzero fill over int cells: byte-addressable, so it goes down the byte walk, but under a
    // typed-cell root rather than a byte pool. Eight bytes covers exactly the middle two elements,
    // so a size read as an element count would stop after two bytes.
    private static int NonzeroFillOverIntCells()
    {
        int[] a = new int[4];
        a[0] = 11;
        a[3] = 22;
        Blk.Init(ref Unsafe.As<int, byte>(ref a[1]), 0xFF, 8);

        if (a[0] != 11) return 14;
        if (a[1] != -1 || a[2] != -1) return 15;
        if (a[3] != 22) return 16;
        return 0;
    }

    // Zeroing one slot of an object[]. This is the row that makes the whole-cell path
    // load-bearing: an object reference has no byte image to read-modify-write, so only a step
    // that writes the destination cell's own zero can serve it. It is a lawful operation on the
    // real runtime — null needs no write barrier — so the oracle can run it. A *nonzero* fill over
    // the same storage would be genuine heap corruption, has no runnable oracle, and PawPrint
    // refuses it loudly instead.
    private static int ZeroFillOverObjectReference()
    {
        object[] o = new object[3];
        object keepFirst = new object();
        object keepLast = new object();
        o[0] = keepFirst;
        o[1] = new object();
        o[2] = keepLast;

        Blk.Init(ref Unsafe.As<object, byte>(ref o[1]), 0, (uint)IntPtr.Size);

        if (!ReferenceEquals(o[0], keepFirst)) return 17;
        if (o[1] != null) return 18;
        if (!ReferenceEquals(o[2], keepLast)) return 19;
        return 0;
    }

    public static int Main(string[] args)
    {
        int r = ByrefIntoArray();
        if (r != 0) return r;
        r = PointerIntoArray();
        if (r != 0) return r;
        r = ValueTruncatesToLowByte();
        if (r != 0) return r;
        r = ZeroCountWritesNothing();
        if (r != 0) return r;
        r = NullWithZeroCount();
        if (r != 0) return r;
        r = NullWithNonzeroCount();
        if (r != 0) return r;
        r = NonzeroFillOverIntCells();
        if (r != 0) return r;
        r = ZeroFillOverObjectReference();
        if (r != 0) return r;
        return 0;
    }
}
"""

    let private runFabricated (expectedOnHost : int) : unit =
        FabricatedGuest.run "Blk" (fabricate ()) "InitblkDriver" driverSource expectedOnHost

    [<Test>]
    let ``initblk agrees with the real runtime on operands C# cannot spell`` () : unit = runFabricated 0
