using System;
using System.Runtime.InteropServices;

public class Program
{
    // A DateTime alongside an `IntPtr` holding a real allocation. The pointer is an
    // ordinary thing for a guest struct to carry, and it is the case that separates
    // reading the source struct *structurally* from flattening it to bytes: PawPrint
    // models an allocated pointer as a managed pointer with provenance, which has no
    // byte rendering, so a whole-struct byte read refuses it. Only the destination is
    // bytes.
    [StructLayout(LayoutKind.Sequential)]
    struct WithPointerAndDate
    {
        public int Id;
        public IntPtr Handle;
        public DateTime When;
    }

    // Native `DateTime` is 8-byte aligned, so `{DateTime, int, DateTime}` leaves four
    // bytes of padding at offset 12. CoreCLR's stub opens the Marshal operation by
    // zeroing the whole native image, so that padding reads as zero even when the
    // destination buffer was dirty beforehand.
    [StructLayout(LayoutKind.Sequential)]
    struct PaddedDates
    {
        public DateTime First;
        public int Middle;
        public DateTime Second;
    }

    public static int Main(string[] args)
    {
        IntPtr scratch = Marshal.AllocHGlobal(16);
        try
        {
            var s = new WithPointerAndDate
            {
                Id = 7,
                Handle = scratch,
                When = new DateTime(2020, 1, 2),
            };

            if (Marshal.SizeOf<WithPointerAndDate>() != 24) return 1;

            IntPtr p = Marshal.AllocHGlobal(Marshal.SizeOf<WithPointerAndDate>());
            try
            {
                Marshal.StructureToPtr(s, p, false);
                if (Marshal.ReadInt32(p, 0) != 7) return 2;
                // The pointer field is deliberately not read back. It marshals correctly
                // — the destination cell holds the pointer with its provenance — but
                // `Marshal.ReadIntPtr` then takes a byte view over that cell, which
                // `executeLdind` refuses. That is an unrelated gap in reading native memory
                // that holds a pointer, not a marshalling one; what this case pins is that a
                // struct carrying a live pointer marshals at all, which a whole-struct byte
                // read of the source made impossible.
                if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 16)) != s.When.ToOADate()) return 4;

                // Marshal into the *same* buffer again. The first pass left a pointer cell at
                // offset 8, and the second must be able to overwrite it. A byte-wise clear of
                // the image cannot: PawPrint models a pointer as a cell with provenance and no
                // byte rendering, and native memory is byte storage, so the whole image is
                // rewritten field by field instead.
                Marshal.StructureToPtr(s, p, false);
                if (Marshal.ReadInt32(p, 0) != 7) return 9;
                if (BitConverter.Int64BitsToDouble(Marshal.ReadInt64(p, 16)) != s.When.ToOADate()) return 10;
            }
            finally
            {
                Marshal.FreeHGlobal(p);
            }
        }
        finally
        {
            Marshal.FreeHGlobal(scratch);
        }

        // Padding must be zeroed by the marshal operation, not left as whatever the
        // buffer held. Dirty the buffer first so a stub that only wrote field ranges
        // would leave 0x7F bytes visible at offset 12.
        int size = Marshal.SizeOf<PaddedDates>();
        if (size != 24) return 5;

        IntPtr q = Marshal.AllocHGlobal(size);
        try
        {
            for (int i = 0; i < size; i++) Marshal.WriteByte(q, i, 0x7F);

            var padded = new PaddedDates
            {
                First = new DateTime(2001, 2, 3),
                Middle = 11,
                Second = new DateTime(2002, 3, 4),
            };
            Marshal.StructureToPtr(padded, q, false);

            if (Marshal.ReadInt32(q, 8) != 11) return 6;
            if (Marshal.ReadInt32(q, 12) != 0) return 7;

            // `DestroyStructure` runs the stub's Cleanup operation, which clears the
            // whole native image after releasing fields — so every byte reads zero.
            Marshal.DestroyStructure<PaddedDates>(q);
            for (int i = 0; i < size; i++)
            {
                if (Marshal.ReadByte(q, i) != 0) return 8;
            }
        }
        finally
        {
            Marshal.FreeHGlobal(q);
        }

        return 0;
    }
}
