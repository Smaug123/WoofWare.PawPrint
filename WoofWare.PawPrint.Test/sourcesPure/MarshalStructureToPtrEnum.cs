using System;
using System.Runtime.InteropServices;

public class Program
{
    enum Wide : int
    {
        Value = 0x01020304,
    }

    enum Narrow : byte
    {
        Value = 7,
    }

    public static int Main(string[] args)
    {
        // An enum has no layout of its own. .NET 10 refuses to marshal one as a structure, with
        // an `ArgumentException`, and this case exits 10 there. .NET 11's `MarshalNative_HasLayout`
        // answers for the enum's underlying integer, so the enum marshals as that integer.
        IntPtr ptr = Marshal.AllocHGlobal(8);
        try
        {
            Marshal.WriteInt64(ptr, -1L);
            try
            {
                Marshal.StructureToPtr((object)Wide.Value, ptr, false);
            }
            catch (ArgumentException)
            {
                return 10;
            }

            if (Marshal.ReadInt32(ptr, 0) != 0x01020304) return 1;
            if (Marshal.ReadInt32(ptr, 4) != -1) return 2;

            Marshal.WriteInt64(ptr, -1L);
            Marshal.StructureToPtr((object)Narrow.Value, ptr, false);
            if (Marshal.ReadByte(ptr, 0) != 7) return 3;
            if (Marshal.ReadByte(ptr, 1) != 0xFF) return 4;

            Marshal.WriteInt32(ptr, 5);
            object back = Marshal.PtrToStructure(ptr, typeof(Wide));
            if (!(back is Wide)) return 5;
            if ((int)(Wide)back != 5) return 6;

            // The underlying integer is blittable, so destroying the image leaves it alone.
            Marshal.WriteInt64(ptr, 0x1122334455667788L);
            Marshal.DestroyStructure(ptr, typeof(Wide));
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 7;

            return 0;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
    }
}
