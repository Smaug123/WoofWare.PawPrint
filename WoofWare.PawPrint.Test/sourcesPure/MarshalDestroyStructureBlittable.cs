using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct Pair
    {
        public int A;
        public int B;
    }

    public static int Main(string[] args)
    {
        // A blittable struct owns no native resources, so `Marshal.DestroyStructure` leaves its
        // native image alone. .NET 11 decides that from the `isBlittable` that the
        // `MarshalNative_HasLayout` QCall answers: told the struct is not blittable, it would free
        // its fields and then clear the image. .NET 10's `MarshalNative_TryGetStructMarshalStub`
        // hands back no stub for a blittable struct, so nothing runs there either.
        IntPtr ptr = Marshal.AllocHGlobal(8);
        try
        {
            Marshal.WriteInt64(ptr, 0x1122334455667788L);
            Marshal.DestroyStructure<Pair>(ptr);
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 1;

            Marshal.DestroyStructure(ptr, typeof(Pair));
            if (Marshal.ReadInt64(ptr) != 0x1122334455667788L) return 2;

            // `fDeleteOld` destroys the old image before writing the new one.
            Marshal.StructureToPtr(new Pair { A = 1, B = 2 }, ptr, true);
            if (Marshal.ReadInt32(ptr, 0) != 1) return 3;
            if (Marshal.ReadInt32(ptr, 4) != 2) return 4;

            return 0;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }
    }
}
