using System;
using System.Runtime.CompilerServices;

// Reading a pointer-typed struct field through a byte view of the struct.
//
// `Unsafe.As<S, byte>(ref s)` hands back a `ref byte` over the whole struct, and
// `Unsafe.ReadUnaligned<IntPtr>` then reads a native int's worth of it. A pointer field has no
// byte image, so the only read such a view can serve over it is one covering exactly that field at
// the width of a native int, and that read must hand over the pointer the field holds -- the same
// value a plain read of the field would give -- so that dereferencing it reaches the original
// pointee. Each root a struct can live in is covered: a local, a class's field, an array element,
// a box, and a field of an enclosing struct; and each pointer shape a field can have: a data
// pointer, `void*`, a function pointer, and an `nint` holding a data pointer.
// `TestPointerFieldByteView.fs` covers the reads which would need the field's bytes, and which
// PawPrint therefore refuses.
//
// Returns 0 on success, or the number of the first check that failed.
public interface ISelfCheck
{
    int CheckSelf (int expectTyped, int expectUntyped, int expectNative, int baseCode);
}

public unsafe struct Pointers : ISelfCheck
{
    public long Before;
    public int* Typed;
    public void* Untyped;
    public delegate*<int, int> Function;
    public nint Native;

    // Called through the interface on a box, `this` is a byref into the box's payload.
    public int CheckSelf (int expectTyped, int expectUntyped, int expectNative, int baseCode) =>
        Program.Check (ref this, expectTyped, expectUntyped, expectNative, baseCode);
}

public unsafe struct Lone
{
    public int* P;
}

public struct Outer
{
    public long Leading;
    public Lone Inner;
}

public class Holder
{
    public Pointers Field;
}

public class Program
{
    static int Triple (int x) => 3 * x;

    public static unsafe int Check (ref Pointers p, int expectTyped, int expectUntyped, int expectNative, int baseCode)
    {
        ref byte data = ref Unsafe.As<Pointers, byte> (ref p);

        IntPtr typed = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.Add (ref data, 8));
        if (*(int*) typed != expectTyped)
            return baseCode + 1;

        IntPtr untyped = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.Add (ref data, 16));
        if (*(int*) untyped != expectUntyped)
            return baseCode + 2;

        IntPtr function = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.Add (ref data, 24));
        if (((delegate*<int, int>) function) (5) != 15)
            return baseCode + 3;

        IntPtr native = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.Add (ref data, 32));
        if (*(int*) native != expectNative)
            return baseCode + 4;

        // The same four again, each read by `ldind.i` through a `ref nint` view, which asks for a
        // bare native int rather than the `IntPtr` wrapper `ReadUnaligned<IntPtr>` asks for.
        nint bareTyped = Unsafe.As<byte, nint> (ref Unsafe.Add (ref data, 8));
        if (*(int*) bareTyped != expectTyped)
            return baseCode + 5;

        nint bareUntyped = Unsafe.As<byte, nint> (ref Unsafe.Add (ref data, 16));
        if (*(int*) bareUntyped != expectUntyped)
            return baseCode + 6;

        nint bareFunction = Unsafe.As<byte, nint> (ref Unsafe.Add (ref data, 24));
        if (((delegate*<int, int>) bareFunction) (5) != 15)
            return baseCode + 7;

        nint bareNative = Unsafe.As<byte, nint> (ref Unsafe.Add (ref data, 32));
        if (*(int*) bareNative != expectNative)
            return baseCode + 8;

        return 0;
    }

    static unsafe Pointers Make (int* typed, int* untyped, int* native)
    {
        Pointers p;
        p.Before = 7;
        p.Typed = typed;
        p.Untyped = untyped;
        p.Function = &Triple;
        p.Native = (nint) native;
        return p;
    }

    public static unsafe int Main (string[] args)
    {
        int a = 11;
        int b = 22;
        int c = 33;

        // 1x: a local.
        Pointers local = Make (&a, &b, &c);
        int result = Check (ref local, 11, 22, 33, 10);
        if (result != 0)
            return result;

        // 2x: a field of a class.
        Holder holder = new Holder ();
        holder.Field = Make (&b, &c, &a);
        result = Check (ref holder.Field, 22, 33, 11, 20);
        if (result != 0)
            return result;

        // 3x: an element of an array of the struct, not the first one.
        Pointers[] array = new Pointers[2];
        array[0] = Make (&a, &a, &a);
        array[1] = Make (&c, &a, &b);
        result = Check (ref array[1], 33, 11, 22, 30);
        if (result != 0)
            return result;

        // 4x: a boxed struct, reached through the box.
        ISelfCheck boxed = Make (&c, &b, &a);
        result = boxed.CheckSelf (33, 22, 11, 40);
        if (result != 0)
            return result;

        // 5x: a field of an enclosing struct, viewed from the enclosing struct.
        Outer outer;
        outer.Leading = 1;
        outer.Inner.P = &b;
        ref byte outerData = ref Unsafe.As<Outer, byte> (ref outer);
        IntPtr nested = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.Add (ref outerData, 8));
        if (*(int*) nested != 22)
            return 51;

        // 52: the same field, viewed from the inner struct, which is exactly one pointer wide.
        IntPtr lone = Unsafe.ReadUnaligned<IntPtr> (ref Unsafe.As<Lone, byte> (ref outer.Inner));
        if (*(int*) lone != 22)
            return 52;

        return 0;
    }
}
