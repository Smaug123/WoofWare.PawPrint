namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// The unsigned branches (`blt.un`, `bge.un`, `bgt.un`, `ble.un`; ECMA-335 III.3.6 to III.3.11)
/// over two pointers into one allocation, against the real runtime. Each orders the pointers by
/// address, as `clt.un` and `cgt.un` do.
///
/// Roslyn compiles a guest's `if (p < q)` to `clt.un` and a `brfalse` when it does not optimise,
/// so a compiled guest does not reach the branches themselves; CoreLib, which is optimised, does
/// (`UnicodeEncoding.GetBytes` loops on `blt.un.s` over a `stackalloc` buffer).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedUnsignedBranchOverPointers =

    /// `Order::BltUn(byte*, byte*) : bool` and its siblings for `bge.un`, `bgt.un` and `ble.un`,
    /// and `Order::BltUnS` and so on for their short forms: each is
    /// `ldarg.0; ldarg.1; <branch> taken; ldc.i4.0; ret; taken: ldc.i4.1; ret`.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Order", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Order"

        let order =
            modul.DefineType ("Order", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static
        let bytePointer = typeof<byte>.MakePointerType ()

        for name, branch in
            [
                "BltUn", OpCodes.Blt_Un
                "BgeUn", OpCodes.Bge_Un
                "BgtUn", OpCodes.Bgt_Un
                "BleUn", OpCodes.Ble_Un
                "BltUnS", OpCodes.Blt_Un_S
                "BgeUnS", OpCodes.Bge_Un_S
                "BgtUnS", OpCodes.Bgt_Un_S
                "BleUnS", OpCodes.Ble_Un_S
            ] do
            let il =
                order.DefineMethod(name, attributes, typeof<bool>, [| bytePointer ; bytePointer |]).GetILGenerator ()

            let taken = il.DefineLabel ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit (branch, taken)
            il.Emit OpCodes.Ldc_I4_0
            il.Emit OpCodes.Ret
            il.MarkLabel taken
            il.Emit OpCodes.Ldc_I4_1
            il.Emit OpCodes.Ret

        order.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Returns the index of the first check that fails, and 0 when every check passes, so a
    /// disagreement names the allocation and the branch that answered wrongly.
    let private driverSource : string =
        """
using System.Runtime.InteropServices;

public struct Pair
{
    public long A;
    public long B;
}

public static unsafe class Driver
{
    // `lo` is below `hi` in one allocation.
    static int Check(byte* lo, byte* hi)
    {
        if (!Order.BltUn(lo, hi) || !Order.BltUnS(lo, hi)) return 1;
        if (Order.BltUn(hi, lo) || Order.BltUnS(hi, lo)) return 2;
        if (Order.BltUn(lo, lo) || Order.BltUnS(lo, lo)) return 3;

        if (Order.BgeUn(lo, hi) || Order.BgeUnS(lo, hi)) return 4;
        if (!Order.BgeUn(hi, lo) || !Order.BgeUnS(hi, lo)) return 5;
        if (!Order.BgeUn(lo, lo) || !Order.BgeUnS(lo, lo)) return 6;

        if (Order.BgtUn(lo, hi) || Order.BgtUnS(lo, hi)) return 7;
        if (!Order.BgtUn(hi, lo) || !Order.BgtUnS(hi, lo)) return 8;
        if (Order.BgtUn(lo, lo) || Order.BgtUnS(lo, lo)) return 9;

        if (!Order.BleUn(lo, hi) || !Order.BleUnS(lo, hi)) return 10;
        if (Order.BleUn(hi, lo) || Order.BleUnS(hi, lo)) return 11;
        if (!Order.BleUn(lo, lo) || !Order.BleUnS(lo, lo)) return 12;

        return 0;
    }

    // `a` and `b` are one address.
    static int CheckEqual(byte* a, byte* b)
    {
        if (Order.BltUn(a, b) || Order.BltUnS(a, b)) return 13;
        if (!Order.BgeUn(a, b) || !Order.BgeUnS(a, b)) return 14;
        if (Order.BgtUn(a, b) || Order.BgtUnS(a, b)) return 15;
        if (!Order.BleUn(a, b) || !Order.BleUnS(a, b)) return 16;
        return 0;
    }

    // `memory` holds at least a `Pair`. The second field's address and the address eight bytes
    // in are one address, reached two ways.
    static int CheckAll(byte* memory)
    {
        int result = Check(memory + 17, memory + 92);
        if (result != 0) return result;
        return CheckEqual((byte*)&((Pair*)memory)->B, memory + 8);
    }

    public static int Main(string[] args)
    {
        int result;

        byte* stack = stackalloc byte[100];
        result = CheckAll(stack);
        if (result != 0) return result;

        byte* native = (byte*)NativeMemory.Alloc(100);
        result = CheckAll(native);
        NativeMemory.Free(native);
        if (result != 0) return 20 + result;

        byte[] array = new byte[100];
        fixed (byte* pinned = array)
        {
            result = Check(pinned + 17, pinned + 92);
        }
        if (result != 0) return 40 + result;

        return 0;
    }
}
"""

    [<Test>]
    let ``the unsigned branches order two pointers into one allocation by address`` () : unit =
        FabricatedGuest.run "Order" (fabricate ()) "OrderDriver" driverSource 0
