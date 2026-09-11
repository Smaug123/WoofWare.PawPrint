namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `add` and `sub` of an integer to a byref that points at an array *element*, which ECMA-335
/// III.1.5 makes byte arithmetic: the operand is a byte count, not an element count.
///
/// No C# source spells this. `T*` arithmetic goes through `conv.u` first, which anchors a byte
/// view on the byref, and `Unsafe.Add` / `Unsafe.AddByteOffset` are intercepted as intrinsics
/// before any `add` is reached. What is left is BCL internals -- `Utf8.FromUtf16` walks a
/// `char*` obtained from `Unsafe.AsPointer(ref MemoryMarshal.GetReference(source))` this way --
/// and fabricated IL.
///
/// Element sizes of 2, 4 and 8 are covered because a runtime that confused byte and element
/// units is exactly right on a `byte[]` and wrong by a factor of the stride on everything else.
/// The reference-array case is here for a different reason: an implementation that reached the
/// answer by way of a byte cursor must still fold a whole-element advance back to an element,
/// because a byref into the middle of an object reference has no byte image to read.
[<TestFixture>]
module TestFabricatedArrayByrefAdd =

    /// A static method per shape, each of them literally `ldarg.0; ldarg.1; add; ldind.*; ret`,
    /// so the driver chooses the byref and the byte count and the fixture contributes nothing but
    /// the instruction.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Adv", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Adv"

        let adv =
            modul.DefineType ("Adv", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let define (name : string) (elementType : Type) (returnType : Type) (op : OpCode) (load : OpCode) : unit =
            let method =
                adv.DefineMethod (name, attributes, returnType, [| elementType.MakeByRefType () ; typeof<int> |])

            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit op
            il.Emit load
            il.Emit OpCodes.Ret

        define "CharAt" typeof<char> typeof<char> OpCodes.Add OpCodes.Ldind_U2
        define "CharBack" typeof<char> typeof<char> OpCodes.Sub OpCodes.Ldind_U2
        // A `ref int` advanced by a count that is not a whole number of elements, read back as
        // the two bytes that live there.
        define "HalfIntAt" typeof<int> typeof<char> OpCodes.Add OpCodes.Ldind_U2
        define "LongAt" typeof<int64> typeof<int64> OpCodes.Add OpCodes.Ldind_I8
        define "RefAt" typeof<string> typeof<string> OpCodes.Add OpCodes.Ldind_Ref

        // `ldarg.0; ldarg.1; add; ldarg.1; sub; ldind.ref; ret` -- out to a mid-cell address and
        // back to the boundary it started from, then read. The arithmetic cancels, so the real
        // runtime reads the original cell whatever the intermediate offset was.
        let defineRoundTrip (name : string) (elementType : Type) (returnType : Type) (load : OpCode) : unit =
            let method =
                adv.DefineMethod (name, attributes, returnType, [| elementType.MakeByRefType () ; typeof<int> |])

            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Add
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Sub
            il.Emit load
            il.Emit OpCodes.Ret

        defineRoundTrip "RefRoundTrip" typeof<string> typeof<string> OpCodes.Ldind_Ref
        defineRoundTrip "IntRoundTrip" typeof<int> typeof<int> OpCodes.Ldind_I4

        adv.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own number on the first check it fails and 0 when it passes, so
    /// a disagreement names the scenario rather than merely reporting one.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    // Two bytes per char: four bytes past `&a[0]` is `a[2]`, and a runtime that read the operand
    // as an element count would land on `a[4]` instead. Both are in bounds and hold different
    // characters, so the two answers are distinguishable rather than one of them faulting.
    private static int CharElement()
    {
        char[] a = new char[] { 'a', 'b', 'c', 'd', 'e' };

        if (Adv.CharAt(ref a[0], 4) != 'c') return 1;
        if (Adv.CharAt(ref a[1], 6) != 'e') return 2;
        if (Adv.CharAt(ref a[3], 0) != 'd') return 3;

        return 0;
    }

    // `sub` routes through the same arithmetic with a negated operand, so it needs its own check:
    // a runtime could get the addition right and still leave the subtraction in element units.
    private static int CharElementBackwards()
    {
        char[] a = new char[] { 'a', 'b', 'c', 'd', 'e' };

        if (Adv.CharBack(ref a[4], 4) != 'c') return 4;
        if (Adv.CharBack(ref a[2], 2) != 'b') return 5;

        return 0;
    }

    // Eight bytes per element, so a stride hardcoded to 2 or 4 shows up here.
    private static int LongElement()
    {
        long[] a = new long[] { 10, 20, 30, 40 };

        if (Adv.LongAt(ref a[0], 8) != 20) return 6;
        if (Adv.LongAt(ref a[0], 24) != 40) return 7;
        if (Adv.LongAt(ref a[3], -16) != 20) return 8;

        return 0;
    }

    // A byte count that is not a whole number of elements: the resulting byref addresses the
    // upper half of `a[0]`, which `ldind.u2` reads as a single 16-bit quantity. Little-endian
    // hosts only, which is every target either runtime supports.
    private static int PartialElement()
    {
        int[] a = new int[] { 0x11223344, 0x55667788 };

        if (Adv.HalfIntAt(ref a[0], 2) != (char) 0x1122) return 9;
        if (Adv.HalfIntAt(ref a[0], 4) != (char) 0x7788) return 10;
        if (Adv.HalfIntAt(ref a[0], 6) != (char) 0x5566) return 11;

        return 0;
    }

    // One pointer per element. The advance is a whole number of elements, so the result must be
    // an element byref again: the cells hold object references, which have no byte image, and a
    // byref left as a byte cursor over one could not be dereferenced at all.
    private static unsafe int ReferenceElement()
    {
        string[] a = new string[] { "zero", "one", "two" };
        int stride = sizeof(IntPtr);

        if (!ReferenceEquals(Adv.RefAt(ref a[0], stride), a[1])) return 12;
        if (!ReferenceEquals(Adv.RefAt(ref a[0], stride * 2), a[2])) return 13;
        if (!ReferenceEquals(Adv.RefAt(ref a[2], 0), a[2])) return 14;

        return 0;
    }

    // Out to a mid-cell address and back again. The two steps cancel before anything is read, so
    // the address dereferenced is the cell boundary it started from -- including for an array of
    // references, whose cells cannot be read a byte at a time at all.
    private static int RoundTripThroughMidCell()
    {
        string[] s = new string[] { "zero", "one" };
        int[] n = new int[] { 11, 22 };

        if (!ReferenceEquals(Adv.RefRoundTrip(ref s[1], 1), s[1])) return 15;
        if (!ReferenceEquals(Adv.RefRoundTrip(ref s[0], 3), s[0])) return 16;
        if (Adv.IntRoundTrip(ref n[1], 1) != 22) return 17;
        if (Adv.IntRoundTrip(ref n[0], 6) != 11) return 18;

        return 0;
    }

    public static int Main()
    {
        int r;
        r = CharElement();
        if (r != 0) return r;
        r = CharElementBackwards();
        if (r != 0) return r;
        r = LongElement();
        if (r != 0) return r;
        r = PartialElement();
        if (r != 0) return r;
        r = ReferenceElement();
        if (r != 0) return r;
        r = RoundTripThroughMidCell();
        if (r != 0) return r;
        return 0;
    }
}
"""

    [<Test>]
    let ``adding an integer to an array element byref counts bytes, as the real runtime does`` () : unit =
        FabricatedGuest.run "Adv" (fabricate ()) "AdvDriver" driverSource 0
