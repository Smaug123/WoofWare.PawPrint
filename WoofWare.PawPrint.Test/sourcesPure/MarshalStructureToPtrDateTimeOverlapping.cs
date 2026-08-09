using System;
using System.Runtime.InteropServices;

public class Program
{
    // Two fields at the same offset, one of them a DateTime. CoreCLR marshals a struct's
    // fields in declaration order, so `Raw` is written after `When` and its bits are what
    // survive — the native image ends up holding the tick count, not the OADate.
    //
    // What this pins for PawPrint is that the marshal reads each field's *effective* value.
    // Assigning one of a set of overlapping fields deliberately leaves its siblings' stored
    // contents stale, and only `DereferenceFieldById` resolves which write covers a given
    // range. Marshalling the stored contents instead would write the OADate and then a stale
    // zero over it, which is neither runtime's answer.
    [StructLayout(LayoutKind.Explicit)]
    struct Overlap
    {
        [FieldOffset(0)] public DateTime When;
        [FieldOffset(0)] public long Raw;
    }

    public static int Main(string[] args)
    {
        var s = new Overlap();
        s.When = new DateTime(2020, 1, 2);

        int size = Marshal.SizeOf<Overlap>();
        if (size != 8) return 1;

        IntPtr p = Marshal.AllocHGlobal(size);
        try
        {
            Marshal.StructureToPtr(s, p, false);
            long bits = Marshal.ReadInt64(p, 0);

            // The later-declared field wins.
            if (bits == s.When.Ticks) return 0;
            // The OADate survived, i.e. declaration order was not honoured.
            if (BitConverter.Int64BitsToDouble(bits) == s.When.ToOADate()) return 2;
            // Neither: most likely a stale sibling value was marshalled.
            return 3;
        }
        finally
        {
            Marshal.FreeHGlobal(p);
        }
    }
}
