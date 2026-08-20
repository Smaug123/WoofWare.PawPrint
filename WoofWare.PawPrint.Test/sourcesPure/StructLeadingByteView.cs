using System;

internal struct V64
{
    public ulong _00;
}

internal struct V128
{
    public V64 _lower;
    public V64 _upper;
}

internal struct V256
{
    public V128 _lower;
    public V128 _upper;
}

internal struct State
{
    public V256 Bitmap;
}

internal struct TwoShortsAndInt
{
    public ushort A;
    public ushort B;
    public uint C;
}

internal static class Program
{
    private static unsafe int Main()
    {
        // A load strictly narrower than the struct a pointer points at reads the
        // struct's leading bytes, however the struct decomposes into fields.
        // This is the shape IndexOfAnyAsciiSearcher.SetBitmapBit relies on: a
        // byte* into Vector128 state, indexed from zero.

        // Zero-displacement byte read through a pointer at a wider struct.
        V64 one = default;
        one._00 = 0x0807060504030201UL;
        byte* p = (byte*)&one;
        if (*p != 0x01)
        {
            return 1;
        }

        // Read-modify-write at index 0: SetBitmapBit's `bitmap[nibble] |= bit`.
        V64 rmw = default;
        rmw._00 = 0xF0UL;
        byte* q = (byte*)&rmw;
        q[0] |= 0x0F;
        if (rmw._00 != 0xFFUL)
        {
            return 2;
        }

        // Pointer to a nested inner field, then index 0: TryComputeBitmap passes
        // (byte*)&state.Bitmap._lower.
        State state = default;
        byte* bitmap = (byte*)&state.Bitmap._lower;
        bitmap[0] |= 1;
        if ((bitmap[0] & 1) == 0)
        {
            return 3;
        }

        if (state.Bitmap._lower._lower._00 != 1UL)
        {
            return 4;
        }

        // A 4-byte narrow read over a 16-byte struct.
        V128 wide = default;
        wide._lower._00 = 0x1122334455667788UL;
        uint* u = (uint*)&wide;
        if (*u != 0x55667788u)
        {
            return 5;
        }

        // A 4-byte read spanning two ushort fields: narrower than the struct but
        // wider than its leading field.
        TwoShortsAndInt spanned = default;
        spanned.A = 0x3412;
        spanned.B = 0x7856;
        uint* v = (uint*)&spanned;
        if (*v != 0x78563412u)
        {
            return 6;
        }

        // Controls: displacement-1 read and write-only store.
        V64 control = default;
        control._00 = 0xAB00UL;
        byte* c = (byte*)&control;
        if (c[1] != 0xAB)
        {
            return 7;
        }

        V64 written = default;
        byte* w = (byte*)&written;
        *w = 9;
        if (written._00 != 9UL)
        {
            return 8;
        }

        // A leading field that exactly covers the window keeps the typed route: a
        // pointer loaded back through a narrow nint window must still be
        // dereferenceable, which a byte rendering could not deliver.
        int x = 7;
        PtrPair pair = default;
        pair.P = &x;
        pair.Tail = 5;
        nint* np = (nint*)&pair;
        nint loaded = *np;
        int* recovered = (int*)loaded;
        if (*recovered != 7 || pair.Tail != 5)
        {
            return 9;
        }

        // An exactly window-sized leading field that is itself decomposed cannot be
        // served by the typed descent either; the byte view must still fire.
        Outer outer = default;
        outer.Head.A = 0x3412;
        outer.Head.B = 0x7856;
        outer.Tail = 1;
        uint* head = (uint*)&outer;
        if (*head != 0x78563412u)
        {
            return 10;
        }

        return 0;
    }
}

internal struct Inner
{
    public ushort A;
    public ushort B;
}

internal struct Outer
{
    public Inner Head;
    public uint Tail;
}

internal unsafe struct PtrPair
{
    public int* P;
    public long Tail;
}
