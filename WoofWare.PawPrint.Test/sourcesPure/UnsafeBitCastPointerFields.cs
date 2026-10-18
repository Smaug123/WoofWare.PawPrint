using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// `Unsafe.BitCast` between two distinct value types that hold managed pointers or object
// references. The bits of a byref are its address, so the cast carries each one across to the
// field at the same offset, now viewed as that field's type; the bits around them are bytes.
// .NET 11's `MemoryExtensions.MinMaxInteger` casts `ReadOnlySpan<char>` to `ReadOnlySpan<ushort>`
// this way.
public class TestUnsafeBitCastPointerFields
{
    private ref struct RefAndInts
    {
        public ref int Target;
        public int Low;
        public int High;
    }

    private ref struct RefAndLong
    {
        public ref uint Target;
        public long Both;
    }

    private struct StringAndInts
    {
        public string Text;
        public int Low;
        public int High;
    }

    private struct ObjectAndLong
    {
        public object Thing;
        public long Both;
    }

    private struct OneObject
    {
        public object Thing;
    }

    // A span's element type changes; its reference and length come across.
    public static int Test1()
    {
        ReadOnlySpan<char> chars = "hello".AsSpan(1);
        ReadOnlySpan<ushort> shorts = Unsafe.BitCast<ReadOnlySpan<char>, ReadOnlySpan<ushort>>(chars);
        if (shorts.Length != 4) return 1;
        if (shorts[0] != 'e') return 2;
        if (shorts[3] != 'o') return 3;

        ushort max = 0;
        foreach (ushort s in shorts)
        {
            if (s > max) max = s;
        }

        if (max != 'o') return 4;

        ReadOnlySpan<ushort> tail = shorts.Slice(2);
        if (tail[0] != 'l') return 5;

        ReadOnlySpan<char> back = Unsafe.BitCast<ReadOnlySpan<ushort>, ReadOnlySpan<char>>(tail);
        if (back.Length != 2) return 6;
        if (back[1] != 'o') return 7;
        if (!Unsafe.AreSame(ref MemoryMarshal.GetReference(back), ref Unsafe.AsRef(in chars[2]))) return 8;

        return 0;
    }

    // A write through the cast span lands in the original storage.
    public static int Test2()
    {
        int[] array = { 1, 2, 3 };
        Span<uint> unsigned = Unsafe.BitCast<Span<int>, Span<uint>>(array.AsSpan());
        unsigned[1] = 0xFFFFFFFFu;
        if (array[1] != -1) return 1;

        Span<byte> bytes = stackalloc byte[4];
        Span<sbyte> signed = Unsafe.BitCast<Span<byte>, Span<sbyte>>(bytes);
        signed[2] = -2;
        if (bytes[2] != 0xFE) return 2;
        if (signed.Length != 4) return 3;

        return 0;
    }

    // Around the byref, the bytes are reassembled into fields of other widths.
    public static int Test3()
    {
        int[] box = { 7 };
        RefAndInts source = default;
        source.Target = ref box[0];
        source.Low = 0x11223344;
        source.High = 0x55667788;

        RefAndLong cast = Unsafe.BitCast<RefAndInts, RefAndLong>(source);
        if (cast.Both != 0x5566778811223344L) return 1;
        if (cast.Target != 7u) return 2;

        cast.Target = 0xFFFFFFF0u;
        if (box[0] != -16) return 3;

        RefAndInts back = Unsafe.BitCast<RefAndLong, RefAndInts>(cast);
        if (back.Low != 0x11223344) return 4;
        if (back.High != 0x55667788) return 5;
        if (!Unsafe.AreSame(ref back.Target, ref box[0])) return 6;

        return 0;
    }

    // An object reference comes across as the same object.
    public static int Test4()
    {
        string text = "pawprint";
        StringAndInts source;
        source.Text = text;
        source.Low = 1;
        source.High = 2;

        ObjectAndLong cast = Unsafe.BitCast<StringAndInts, ObjectAndLong>(source);
        if (!ReferenceEquals(cast.Thing, text)) return 1;
        if (cast.Both != 0x0000000200000001L) return 2;

        return 0;
    }

    // A null reference's bits are zero.
    public static int Test5()
    {
        OneObject nothing = default;
        if (Unsafe.BitCast<OneObject, long>(nothing) != 0L) return 1;

        return 0;
    }

    public static int Main(string[] argv)
    {
        var result = Test1();
        if (result != 0) return 10 + result;

        result = Test2();
        if (result != 0) return 20 + result;

        result = Test3();
        if (result != 0) return 30 + result;

        result = Test4();
        if (result != 0) return 40 + result;

        result = Test5();
        if (result != 0) return 50 + result;

        return 0;
    }
}
