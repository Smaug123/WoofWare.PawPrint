using System;
using System.Runtime.InteropServices;

public class Program
{
    // Two `MARSHAL_TYPE_DATE` fields, so the marshal stub has to run more than one
    // conversion. PawPrint drives those by re-executing the `calli` once per conversion
    // (see `StructMarshalStub`), which the single-DateTime sibling
    // `MarshalStructureToPtrDateTimeField.cs` cannot exercise: it completes in one pass.
    // The `int` between them also pins that a conversion field does not disturb the
    // placement of an ordinary one.
    [StructLayout(LayoutKind.Sequential)]
    struct TwoDates
    {
        public DateTime First;
        public int Middle;
        public DateTime Second;
    }

    [StructLayout(LayoutKind.Sequential)]
    struct JustADate
    {
        public DateTime When;
    }

    public static int Main(string[] args)
    {
        var two = new TwoDates
        {
            First = new DateTime(1999, 12, 31),
            Middle = 42,
            Second = new DateTime(2024, 6, 5),
        };

        // Native `DateTime` is an 8-byte OADate at 8-byte alignment, so the `int` sits at 8
        // and the second date is pushed out to 16: 24 bytes, not the 20 a 4-aligned walk
        // would give.
        if (Marshal.SizeOf<TwoDates>() != 24) return 1;

        IntPtr p = Marshal.AllocHGlobal(Marshal.SizeOf<TwoDates>());
        try
        {
            Marshal.StructureToPtr(two, p, false);
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 0)) != two.First.ToOADate()) return 2;
            if (Marshal.ReadInt32(p, 8) != 42) return 3;
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 16)) != two.Second.ToOADate()) return 4;

            // Belt-and-braces, as in the sibling file: these must not be the managed
            // `_dateData` tick counts.
            if (Marshal.ReadInt64(p, 0) == two.First.Ticks) return 5;

            // `fDeleteOld: true` makes CoreLib invoke the stub twice — once with
            // `MarshalOperation.Cleanup` over the existing contents, then with
            // `MarshalOperation.Marshal`. Nothing a DateTime field owns needs releasing, so
            // the result must be identical to the pass above.
            Marshal.StructureToPtr(two, p, true);
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 0)) != two.First.ToOADate()) return 6;
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 16)) != two.Second.ToOADate()) return 7;
        }
        finally
        {
            Marshal.FreeHGlobal(p);
        }

        // `default(DateTime)` is the case that proves the conversion runs CoreLib's own
        // `ToOADate` rather than a host-side transcription of the arithmetic:
        // `TicksToOADate` (DateTime.cs:1663) special-cases zero ticks to 0.0, whereas the
        // general formula would give the OLE Automation epoch offset instead.
        var one = new JustADate { When = default };
        if (Marshal.SizeOf<JustADate>() != 8) return 8;

        IntPtr q = Marshal.AllocHGlobal(8);
        try
        {
            Marshal.StructureToPtr(one, q, false);
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(q, 0)) != 0.0) return 9;
        }
        finally
        {
            Marshal.FreeHGlobal(q);
        }

        return 0;
    }
}
