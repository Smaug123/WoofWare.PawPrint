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

    // Two conversion fields with a copied field between them, so unmarshalling has to resume
    // after each DateTime and still land every field in its own slot.
    [StructLayout(LayoutKind.Sequential)]
    struct TwoDates
    {
        public DateTime First;
        public int Middle;
        public DateTime Second;
    }

    // Declaration order decides overlapping fields in both directions: on the way out `Raw` is
    // written after `When`, so the native image holds the tick count; on the way back `When` is
    // unmarshalled first (from those bits read as a double) and `Raw` then overwrites it.
    [StructLayout(LayoutKind.Explicit)]
    struct RawLast
    {
        [FieldOffset(0)] public DateTime When;
        [FieldOffset(0)] public long Raw;
    }

    // The reverse: the native image holds the OA date, and `When` is unmarshalled last.
    [StructLayout(LayoutKind.Explicit)]
    struct DateLast
    {
        [FieldOffset(0)] public long Raw;
        [FieldOffset(0)] public DateTime When;
    }

    const int BufferSize = 32;

    // The native form of a DateTime field is an OLE Automation date: an 8-byte double counting
    // days from 1899-12-30. Write one by hand, bypassing StructureToPtr, and read it back.
    static WithDateTime FromNative(IntPtr ptr, int id, double oaDate)
    {
        Marshal.WriteInt32(ptr, 0, id);
        Marshal.WriteInt64(ptr, 8, BitConverter.DoubleToInt64Bits(oaDate));
        return Marshal.PtrToStructure<WithDateTime>(ptr);
    }

    static int CheckNative(IntPtr ptr, double oaDate, long expectedTicks)
    {
        WithDateTime r = FromNative(ptr, 3, oaDate);
        if (r.Id != 3) return 1;
        if (r.When.Ticks != expectedTicks) return 2;
        if (r.When.Kind != DateTimeKind.Unspecified) return 3;
        return 0;
    }

    static bool Rejects(IntPtr ptr, double oaDate)
    {
        try
        {
            FromNative(ptr, 4, oaDate);
            return false;
        }
        catch (ArgumentException)
        {
            return true;
        }
    }

    // Deterministic LCG, so the sweep is the same on every run and on every runtime.
    static ulong state = 0x9E3779B97F4A7C15;

    static ulong Next()
    {
        state = state * 6364136223846793005UL + 1442695040888963407UL;
        return state;
    }

    public static int Main(string[] args)
    {
        if (Marshal.SizeOf<WithDateTime>() != 16) return 1;
        if (Marshal.SizeOf<TwoDates>() != 24) return 2;

        IntPtr ptr = Marshal.AllocHGlobal(BufferSize);
        try
        {
            // Tick counts measured on real .NET. Negative OA dates count whole days backwards
            // but the fraction forwards, so -1.25 is 06:00 on 1899-12-29, not 18:00 on 1899-12-28.
            int r;
            if ((r = CheckNative(ptr, 0.0, 599264352000000000L)) != 0) return 10 + r;
            if ((r = CheckNative(ptr, -1.25, 599263704000000000L)) != 0) return 20 + r;
            if ((r = CheckNative(ptr, 1.5, 599265648000000000L)) != 0) return 30 + r;
            if ((r = CheckNative(ptr, -0.5, 599264784000000000L)) != 0) return 40 + r;
            if ((r = CheckNative(ptr, 2958465.99999999, 3155378975999990000L)) != 0) return 50 + r;
            if ((r = CheckNative(ptr, -657434.0, 31241376000000000L)) != 0) return 60 + r;
            if ((r = CheckNative(ptr, 36526.123456789, 630822922666670000L)) != 0) return 70 + r;

            // Outside the OLE Automation range, `DateMarshaler.ConvertToManaged` throws, and the
            // guest sees it from PtrToStructure.
            if (!Rejects(ptr, double.NaN)) return 80;
            if (!Rejects(ptr, 2958466.0)) return 81;
            if (!Rejects(ptr, -657435.0)) return 82;
            if (!Rejects(ptr, double.PositiveInfinity)) return 83;

            // A round trip keeps whole milliseconds and drops the Kind.
            var utc = new DateTime(2020, 1, 2, 3, 4, 5, DateTimeKind.Utc).AddTicks(12345);
            Marshal.StructureToPtr(new WithDateTime { Id = 5, When = utc }, ptr, false);
            var back = Marshal.PtrToStructure<WithDateTime>(ptr);
            if (back.Id != 5) return 90;
            if (back.When.Ticks != 637135310450010000L) return 91;
            if (back.When.Kind != DateTimeKind.Unspecified) return 92;

            // DateTime.MinValue goes out as OA date 0, which is 1899-12-30, not MinValue.
            Marshal.StructureToPtr(new WithDateTime { Id = 6, When = DateTime.MinValue }, ptr, false);
            back = Marshal.PtrToStructure<WithDateTime>(ptr);
            if (back.When.Ticks != 599264352000000000L) return 93;

            // A sweep over the OLE Automation range. The property is that a round trip agrees
            // with ToOADate followed by FromOADate; the checksum pins the recovered tick counts
            // themselves against real .NET, independently of those two methods.
            const long minTicks = 31241376000000000L; // 0100-01-01
            const long maxTicks = 3155378975999999999L; // 9999-12-31T23:59:59.9999999
            ulong checksum = 0;
            for (int i = 0; i < 40; i++)
            {
                long firstTicks = minTicks + (long)(Next() % (ulong)(maxTicks - minTicks));
                long secondTicks = minTicks + (long)(Next() % (ulong)(maxTicks - minTicks));
                var first = new DateTime(firstTicks, (DateTimeKind)(i % 3));
                var second = new DateTime(secondTicks);
                Marshal.StructureToPtr(new TwoDates { First = first, Middle = i, Second = second }, ptr, false);
                var two = Marshal.PtrToStructure<TwoDates>(ptr);
                if (two.Middle != i) return 100;
                if (two.First != DateTime.FromOADate(first.ToOADate())) return 101;
                if (two.Second != DateTime.FromOADate(second.ToOADate())) return 102;
                if (two.First.Kind != DateTimeKind.Unspecified) return 103;
                checksum = checksum * 31 + (ulong)two.First.Ticks;
                checksum = checksum * 31 + (ulong)two.Second.Ticks;
            }
            if (checksum != 11128051525419662544UL) return 104;

            var day = new DateTime(2020, 1, 2);

            Marshal.StructureToPtr(new RawLast { When = day }, ptr, false);
            var rawLast = Marshal.PtrToStructure<RawLast>(ptr);
            if (rawLast.Raw != day.Ticks) return 110;
            if (rawLast.When != day) return 111;

            Marshal.StructureToPtr(new DateLast { When = day }, ptr, false);
            if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(ptr, 0)) != day.ToOADate()) return 112;
            var dateLast = Marshal.PtrToStructure<DateLast>(ptr);
            if (dateLast.When != day) return 113;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
        return 0;
    }
}
