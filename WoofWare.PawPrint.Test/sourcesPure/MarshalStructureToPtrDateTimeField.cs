using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct WithDateTime
    {
        public int Id;
        public DateTime When;
    }

    public static int Main(string[] args)
    {
        // CoreCLR's `MarshalInfo::MarshalInfo` (mlinfo.cpp:1747) special-cases
        // `System.DateTime` as `MARSHAL_TYPE_DATE` before the AutoLayout
        // rejection: a sequential struct field typed `DateTime` marshals to
        // native as an 8-byte OADate (a little-endian IEEE-754 double whose
        // value is `dt.ToOADate()`), NOT as the managed `ulong _dateData`
        // field's bytes. PawPrint's `MarshalNative_TryGetStructMarshalStub`
        // `isStrictlyNumericBlittable` arm currently recurses into `DateTime`,
        // sees its single `ulong _dateData` field, declares the parent struct
        // blittable, and would take the memmove fast path — silently writing
        // the managed `_dateData` bytes instead of OADate bytes. This test
        // pins the expected behaviour and tracks the future OADate-conversion
        // stub work.
        var s = new WithDateTime { Id = 7, When = new DateTime(2020, 1, 2) };
        int size = Marshal.SizeOf<WithDateTime>();
        IntPtr ptr = Marshal.AllocHGlobal(size);
        try
        {
            Marshal.StructureToPtr(s, ptr, false);
            if (Marshal.ReadInt32(ptr, 0) != 7) return 1;
            long bits = Marshal.ReadInt64(ptr, 8);
            double actual = BitConverter.Int64BitsToDouble(bits);
            double expected = s.When.ToOADate();
            if (actual != expected) return 2;
            // Belt-and-braces: ensure these aren't the managed `_dateData`
            // bytes. `_dateData` for `new DateTime(2020,1,2)` is the tick
            // count 637135200000000000 with the kind bits cleared.
            if (bits == 637135200000000000L) return 3;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
