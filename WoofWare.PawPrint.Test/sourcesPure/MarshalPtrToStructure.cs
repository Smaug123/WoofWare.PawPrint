using System;
using System.Runtime.InteropServices;

public class Program
{
    [StructLayout(LayoutKind.Sequential)]
    struct BlittableStruct
    {
        public int X;
        public double Y;
        public long Z;
    }

    public static int Main(string[] args)
    {
        // Split out of AdvancedStructLayout.cs, which passes in full except for this
        // round trip. The write direction (`Marshal.StructureToPtr`) works today; the
        // read direction does not, because the non-generic
        // `Marshal.PtrToStructure(IntPtr, Type)` overload allocates its result via
        // `Activator.CreateInstance(structureType, nonPublic: true)` (Marshal.cs:572)
        // before doing any marshalling at all. That lands in
        // `RuntimeType.CreateInstanceDefaultCtor` -> `RuntimeType.ActivatorCache` ->
        // the `RuntimeTypeHandle_GetActivationInfo` QCall, which PawPrint does not
        // implement. See TestPureCases.fs for the full blocker chain.
        var blittable = new BlittableStruct { X = 100, Y = 200.5, Z = 300 };
        IntPtr ptr = Marshal.AllocHGlobal(Marshal.SizeOf(typeof(BlittableStruct)));
        try
        {
            Marshal.StructureToPtr(blittable, ptr, false);
            var recovered = (BlittableStruct)Marshal.PtrToStructure(ptr, typeof(BlittableStruct));

            if (recovered.X != 100) return 1;
            if (Math.Abs(recovered.Y - 200.5) > 0.00001) return 2;
            if (recovered.Z != 300) return 3;
        }
        finally
        {
            Marshal.FreeHGlobal(ptr);
        }

        return 0;
    }
}
